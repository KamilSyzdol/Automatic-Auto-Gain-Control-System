'24.11.2023
'uC nr 1

$regfile = "m8adef.dat"
$crystal = 1000000

Dim X As Byte                                               'tymczasowe
Dim Y As Byte

Dim Tmp_gain1 As Byte                                       'tymczasowe zmienne do multipleksowania programowego
Dim Tmp_gain2 As Byte

'adc
Declare Function Adc2db(byval Adc2db_dbl As Word) As Byte
Declare Function Pomiaradc(byval Adc_pomiar_kanal As Byte) As Byte
Config Adc = Single , Prescaler = Auto , Reference = Internal
Start Adc


Config Portd.2 = Input
Portd.2 = 1                                                 'podciagniecie

'przerwanie co 50ms
Config Timer0 = Timer , Prescale = 256                      '256 powinno byc
On Timer0 Przerwanie
Enable Interrupts
Enable Timer0
Start Timer0
Timer0 = 60

'token ring
Busy_out Alias Portb.1
Config Busy_out = Output

Declare Sub Ram_page_write(byval Ram_page_write_param As Byte , Byval Ram_page_write_adr As Byte , Byval Ram_page_write_ile As Byte)
Declare Sub Ram_page_read(byval Ram_page_read_param As Byte , Byval Ram_page_read_adr As Byte , Byval Ram_page_read_ile As Byte)


'zmienne przechowujace rejestry R23 i R21 na czas wstawek assemblerowych
Dim Ram_r23 As Byte
Dim Ram_r21 As Byte

'sterowanie RAM
Ram_cs Alias Portb.0
Ram_clk Alias Portd.7
Ram_sdo Alias Portd.6                                       'podlaczyc do SDO pamieci
Ram_sdi Alias Portd.5                                       'podlaczyc do SDI pamieci

'konfiguracja pinow
Config Ram_cs = Input
Config Ram_clk = Input
Config Ram_sdi = Input
Config Ram_sdo = Input

'wyjscie kontrolne
Config Portd.1 = Output

'tu beda przechowywane zmienne odczytane z RAM
Dim Parametr_odczyt(55) As Byte

'tu beda przechowywane zmienne do zapisu do RAM
Dim Parametr(20) As Byte


Declare Sub Calc_ag(byval Calc_ag_kanal As Byte , Byval Calc_ag_lvl As Byte) As Byte
Declare Function Add_db(byval Add_db_a As Byte , Byval Add_db_b As Byte) As Byte


'dcaf pinout
'1 Clk Pd3 | 2 Dat Pd0 | 3 gnd gnd | 4 +5V +5V | 5 cs PD4 | 6 str PB2
Declare Sub Pga(byval Pga_r As Byte , Byval Pga_l As Byte)
Dcaf_clk Alias Portd.3 : Dcaf_dat Alias Portd.0 : Dcaf_cs Alias Portd.4 : Dcaf_str Alias Portb.2
Config Dcaf_clk = Output : Config Dcaf_dat = Output : Config Dcaf_cs = Output : Config Dcaf_str = Output


'wartosci eq_dat powinny byc w przedziale 0-16
Declare Sub Eq(byval Eq_dat1 As Byte , Byval Eq_dat2 As Byte , Byval Eq_dat3 As Byte , Byval Eq_dat4 As Byte)

'zmienne wywolania programowego przerwania kazdego kanalu
'speed 0=50ms, 1=50ms, 2=100ms, 3=150ms, 4=200ms...
Dim Speed(2) As Byte
Dim Speed_tact(2) As Byte

Speed(1) = 11                                               'tymczasowo, docelowo algorytm sam bedzie wybieral z tablicy ktora predkosc
Speed(2) = 11

Speed_tact(1) = 0
Speed_tact(2) = 0

Dim Srednia_tabela_l(20) As Byte
Dim Srednia_tabela_r(20) As Byte
Dim Suma_srednia_l As Word
Dim Suma_srednia_r As Word

'prog zadzialania algorytmu AGC
Dim Threshold(2) As Byte
Threshold(1) = 35 : Threshold(2) = 35

'poziom docelowy, do ktorego dazy algorytm
Dim Lvl_docelowy(2) As Byte
Lvl_docelowy(1) = 50 : Lvl_docelowy(2) = 50

'ile maksymalnie dB algorytm moze wzmocnic sygnal wejsciowy
Dim Ag_max(2) As Byte
Ag_max(1) = 83 : Ag_max(2) = 83

'szybkosc narastania i opadania sygnalu dla roznych przedzialow
Dim Section1_up_speed(2) As Byte : Dim Section1_down_speed(2) As Byte
Dim Section2_up_speed(2) As Byte : Dim Section2_down_speed(2) As Byte
Dim Section3_up_speed(2) As Byte : Dim Section3_down_speed(2) As Byte
Dim Section4_up_speed(2) As Byte : Dim Section4_down_speed(2) As Byte
Dim Section5_up_speed(2) As Byte : Dim Section5_down_speed(2) As Byte


'przedzialy poszczegolnych sekcji        ( granica przedzialu
Dim Kanal1_section_lvl(5) As Byte : Dim Kanal2_section_lvl(5) As Byte


Declare Function Lvl2sect(byval Lvl2sect_lvl As Byte , Byval Lvl2sect_dest As Byte , Byval Lvl2sect_kanal As Byte) As Byte

'poziom wzmocnienia
Dim Ag_gain(2) As Byte
Ag_gain(1) = 63 : Ag_gain(2) = 63


'funkcja obliczania maksymalnego gainu z powodu ratio
Declare Function Ratio_gain(byval Ratio_gain_wejsciowy As Byte , Byval Ratio_gain_docelowy As Byte , Byval Ratio_gain_ratio As Byte) As Byte


'poziom wyjsciowy
Dim Lvl_wyjsciowy(2) As Byte
Lvl_wyjsciowy(1) = 63 : Lvl_wyjsciowy(2) = 63


Dim Bypass As Byte
Bypass = 1                                                  '0=algorytm nie dziala

Do

Loop




Przerwanie:
    Timer0 = 60                                             'program wykona sie co 50ms dla zegara 1MHz

    'nadanie znaku rozpoczecia cyklu - diagnostycznie
    Portd.1 = 0
    Waitus 200
    Portd.1 = 1

    Dcaf_cs = 0
    Waitus 500
    Dcaf_cs = 1


    'dostep do pamieci RAM
    If Pind.2 = 1 Then
       'pamiec RAM zajeta przez inny uC
    Else
       'pamiec RAM wolna, mozna zajac
      Busy_out = 1                                          'token ring
      Call Ram_page_read(1 , 1 , 55)                        'odczyt parametrow
      Call Ram_page_write(1 , 140 , 20)                     'nadawanie statusu
      Busy_out = 0                                          'token ring
    End If

         'inkrementacja zmiennych do wyzwalania cyklow co okreslony czas
          Speed_tact(1) = Speed_tact(1) + 1
          Speed_tact(2) = Speed_tact(2) + 1


          'pomiar z ADC poziomow sygnalow za filtrami
          X = Pomiaradc(0)                                  '4 lewy srodek
          Parametr(2) = X

          X = Pomiaradc(1)
          Parametr(1) = X                                   'lewy bas

          X = Pomiaradc(2)                                  'prawy bas
          Parametr(4) = X

          X = Pomiaradc(3)
          Parametr(3) = X                                   '6 lewa gora

          X = Pomiaradc(4)                                  '3prawa gora
          Parametr(6) = X

          X = Pomiaradc(5)                                  '2  prawy srodek
          Parametr(5) = X


          'wyznaczanie poziomu na podstawie kilku pomiarow
          Suma_srednia_l = Suma_srednia_l + Parametr(2)
          Suma_srednia_r = Suma_srednia_r + Parametr(5)

          'parametry wyjsciowe do nadania do RAM
          Parametr(9) = Speed(1)
          Parametr(10) = Speed(2)
          Parametr(11) = Speed_tact(1)
          Parametr(12) = Speed_tact(2)



          'aktualizacja wartosci parametrow----------------------------------------------------------


'ponizej tego progu algorytm nie zmienia nic
Threshold(1) = Parametr_odczyt(2) + 33 : Threshold(2) = Parametr_odczyt(7) + 33

'poziom do ktorego dazy algorytm (endLvl)
Lvl_docelowy(1) = Parametr_odczyt(3) + 48 : Lvl_docelowy(2) = Parametr_odczyt(8) + 48

'maksymalne dopuszczalne wzmocnienie przez algorytm
Ag_max(1) = Parametr_odczyt(4) + 53 : Ag_max(2) = Parametr_odczyt(9) + 53

'szybkosc reakcji dla danej sekcji
Section1_up_speed(1) = Parametr_odczyt(11) + 1 : Section1_down_speed(1) = Parametr_odczyt(21) + 1 : Section1_up_speed(2) = Parametr_odczyt(16) + 1 : Section1_down_speed(2) = Parametr_odczyt(26) + 1
Section2_up_speed(1) = Parametr_odczyt(12) + 1 : Section2_down_speed(1) = Parametr_odczyt(22) + 1 : Section2_up_speed(2) = Parametr_odczyt(17) + 1 : Section2_down_speed(2) = Parametr_odczyt(27) + 1
Section3_up_speed(1) = Parametr_odczyt(13) + 1 : Section3_down_speed(1) = Parametr_odczyt(23) + 1 : Section3_up_speed(2) = Parametr_odczyt(18) + 1 : Section3_down_speed(2) = Parametr_odczyt(28) + 1
Section4_up_speed(1) = Parametr_odczyt(14) + 1 : Section4_down_speed(1) = Parametr_odczyt(24) + 1 : Section4_up_speed(2) = Parametr_odczyt(19) + 1 : Section4_down_speed(2) = Parametr_odczyt(29) + 1
Section5_up_speed(1) = Parametr_odczyt(15) + 1 : Section5_down_speed(1) = Parametr_odczyt(25) + 1 : Section5_up_speed(2) = Parametr_odczyt(20) + 1 : Section5_down_speed(2) = Parametr_odczyt(30) + 1

'przynaleznosc do sekcji zalezy od ROZNICY miedzy sygnalem docelowym a wejsciowym
Kanal1_section_lvl(1) = Parametr_odczyt(31) + 42 : Kanal2_section_lvl(1) = Parametr_odczyt(36) + 42
Kanal1_section_lvl(2) = Parametr_odczyt(32) + 42 : Kanal2_section_lvl(2) = Parametr_odczyt(37) + 42
Kanal1_section_lvl(3) = Parametr_odczyt(33) + 42 : Kanal2_section_lvl(3) = Parametr_odczyt(38) + 42
Kanal1_section_lvl(4) = Parametr_odczyt(34) + 42 : Kanal2_section_lvl(4) = Parametr_odczyt(39) + 42
Kanal1_section_lvl(5) = Parametr_odczyt(35) + 42 : Kanal2_section_lvl(5) = Parametr_odczyt(40) + 42

'jesli 0 (czyli wyswietla sie -inf, to nieskonczonosc traktujemy jako 33 (czyli -30dB)
If Parametr_odczyt(31) = 0 Then Kanal1_section_lvl(1) = 33 : If Parametr_odczyt(36) = 0 Then Kanal2_section_lvl(1) = 33
If Parametr_odczyt(32) = 0 Then Kanal1_section_lvl(2) = 33 : If Parametr_odczyt(37) = 0 Then Kanal2_section_lvl(2) = 33
If Parametr_odczyt(33) = 0 Then Kanal1_section_lvl(3) = 33 : If Parametr_odczyt(38) = 0 Then Kanal2_section_lvl(3) = 33
If Parametr_odczyt(34) = 0 Then Kanal1_section_lvl(4) = 33 : If Parametr_odczyt(39) = 0 Then Kanal2_section_lvl(4) = 33
If Parametr_odczyt(35) = 0 Then Kanal1_section_lvl(5) = 33 : If Parametr_odczyt(40) = 0 Then Kanal2_section_lvl(5) = 33

'wzmocnienie ratio zalezne od sekcji
Dim Ag_ratio_kanal1(5) As Byte : Dim Ag_ratio_kanal2(5) As Byte
Ag_ratio_kanal1(1) = Parametr_odczyt(41) + 1 : Ag_ratio_kanal2(1) = Parametr_odczyt(46) + 1
Ag_ratio_kanal1(2) = Parametr_odczyt(42) + 1 : Ag_ratio_kanal2(2) = Parametr_odczyt(47) + 1
Ag_ratio_kanal1(3) = Parametr_odczyt(43) + 1 : Ag_ratio_kanal2(3) = Parametr_odczyt(48) + 1
Ag_ratio_kanal1(4) = Parametr_odczyt(44) + 1 : Ag_ratio_kanal2(4) = Parametr_odczyt(49) + 1
Ag_ratio_kanal1(5) = Parametr_odczyt(45) + 1 : Ag_ratio_kanal2(5) = Parametr_odczyt(50) + 1

          '------------------------------------------------------------------------------------------



             'kanal 1 - jesli juz pora na kolejny cykl
             If Speed_tact(1) >= Speed(1) Then

                Speed_tact(1) = 0
                Suma_srednia_l = Suma_srednia_l / Speed(1)  'obliczony sredni poziom
                Parametr(7) = Suma_srednia_l                'parametr do nadania do RAM i algorytmu

                'kanal i poziom
                Call Calc_ag(1 , Parametr(2))
                Parametr(19) = Ag_gain(1)

               Suma_srednia_l = 0                           'zerowanie na potrzeby kolejnych pomiarow
             End If


             'kanal 2 - jesli juz pora na kolejny cykl
             If Speed_tact(2) >= Speed(2) Then

               Speed_tact(2) = 0
               Suma_srednia_r = Suma_srednia_r / Speed(2)   'obliczony sredni poziom
               Parametr(8) = Suma_srednia_r                 'parametr do nadania do RAM i algorytmu

                'kanal i poziom
                Call Calc_ag(2 , Parametr(5))
                Parametr(20) = Ag_gain(2)

               Suma_srednia_l = 0                           'zerowanie na potrzeby kolejnych pomiarow
             End If


      'funkcja bypass
      If Parametr_odczyt(5) <> 1 Then Call Pga(63 , 63)     'bypass = gain 0dB

      If Parametr_odczyt(5) = 1 Then                        'multipleksowanie programowe

         'kanal 1
            If Parametr_odczyt(1) = 0 Then Tmp_gain1 = Parametr(20)
            If Parametr_odczyt(1) = 1 Then Tmp_gain1 = Parametr(19)
            If Parametr_odczyt(1) = 2 Then
               'wybieram mniejszy
               Tmp_gain1 = Parametr(20)
               If Parametr(20) > Parametr(19) Then Tmp_gain1 = Parametr(19)
            End If
            If Parametr_odczyt(1) = 3 Then
            'wybieram wiekszy
            Tmp_gain1 = Parametr(20)
            If Parametr(20) < Parametr(19) Then Tmp_gain1 = Parametr(19)
            End If
            If Parametr_odczyt(1) = 4 Then Tmp_gain1 = 63


         'kanal 2
            If Parametr_odczyt(6) = 0 Then Tmp_gain2 = Parametr(19)
            If Parametr_odczyt(6) = 1 Then Tmp_gain2 = Parametr(20)
            If Parametr_odczyt(6) = 2 Then
               'wybieram mniejszy
               Tmp_gain2 = Parametr(19)
               If Parametr(19) > Parametr(20) Then Tmp_gain2 = Parametr(20)
            End If
            If Parametr_odczyt(6) = 3 Then
            'wybieram wiekszy
            Tmp_gain2 = Parametr(19)
            If Parametr(19) < Parametr(20) Then Tmp_gain2 = Parametr(20)
            End If
            If Parametr_odczyt(6) = 4 Then Tmp_gain2 = 63


            Call Pga(tmp_gain1 , Tmp_gain2 )

      End If

      'nadawanie konfiguracji filtra do 4094
      Call Eq(parametr_odczyt(52) , Parametr_odczyt(51) , Parametr_odczyt(54) , Parametr_odczyt(53))



         'nadanie znaku zakonczenia cyklu - diagnostycznie
         Portd.1 = 0
         Waitus 200
         Portd.1 = 1
         Waitus 200
         Portd.1 = 0
         Waitus 200
         Portd.1 = 1
Return



Sub Calc_ag(byval Calc_ag_kanal As Byte , Byval Calc_ag_lvl As Byte)

   Dim Calc_ag_sect As Byte

   Lvl_wyjsciowy(calc_ag_kanal) = Calc_ag_lvl + Ag_gain(calc_ag_kanal)
   Lvl_wyjsciowy(calc_ag_kanal) = Lvl_wyjsciowy(calc_ag_kanal) - 63

   'sprawdzam czy poziom wejsciowy przekracza parametr threshold
   If Threshold(calc_ag_kanal) <= Calc_ag_lvl Then

      'ustalam w ktorej sekcji sie znajduje
      'poziom wej, docelowy, kanal
      Calc_ag_sect = Lvl2sect(calc_ag_lvl , Lvl_docelowy(calc_ag_kanal) , Calc_ag_kanal)

      'jesli poziom wyjsciowy jest mniejszy niz docelowy
      If Lvl_wyjsciowy(calc_ag_kanal) < Lvl_docelowy(calc_ag_kanal) Then

         'trzeba ustalic nowy speed
         'trzeba zwiekszyc wzmocnienie w ustalonych granicach (ratio i max ag)

         If Calc_ag_kanal = 1 Then
            Parametr(17) = Lvl_wyjsciowy(calc_ag_kanal)
         End If
         If Calc_ag_kanal = 2 Then
            Parametr(18) = Lvl_wyjsciowy(calc_ag_kanal)
         End If

         If Calc_ag_sect = 1 Then Speed(calc_ag_kanal) = Section1_up_speed(calc_ag_kanal)
         If Calc_ag_sect = 2 Then Speed(calc_ag_kanal) = Section2_up_speed(calc_ag_kanal)
         If Calc_ag_sect = 3 Then Speed(calc_ag_kanal) = Section3_up_speed(calc_ag_kanal)
         If Calc_ag_sect = 4 Then Speed(calc_ag_kanal) = Section4_up_speed(calc_ag_kanal)
         If Calc_ag_sect = 5 Then Speed(calc_ag_kanal) = Section5_up_speed(calc_ag_kanal)


         'warunek ratio i max wzmocnienia dla obu kanalow
         If Calc_ag_kanal = 1 Then
            If Ag_gain(calc_ag_kanal) < Ag_max(calc_ag_kanal) And Ag_gain(calc_ag_kanal) < Ratio_gain(calc_ag_lvl , Lvl_docelowy(calc_ag_kanal) , Ag_ratio_kanal1(calc_ag_sect)) Then Ag_gain(calc_ag_kanal) = Ag_gain(calc_ag_kanal) + 1
         End If

         If Calc_ag_kanal = 2 Then
            If Ag_gain(calc_ag_kanal) < Ag_max(calc_ag_kanal) And Ag_gain(calc_ag_kanal) < Ratio_gain(calc_ag_lvl , Lvl_docelowy(calc_ag_kanal) , Ag_ratio_kanal2(calc_ag_sect)) Then Ag_gain(calc_ag_kanal) = Ag_gain(calc_ag_kanal) + 1
         End If

      End If


      'jesli poziom wyjsciowy jest wiekszy niz docelowy
      If Lvl_wyjsciowy(calc_ag_kanal) > Lvl_docelowy(calc_ag_kanal) Then

         If Calc_ag_kanal = 1 Then
            Parametr(17) = Lvl_wyjsciowy(calc_ag_kanal)
         End If
         If Calc_ag_kanal = 2 Then
            Parametr(18) = Lvl_wyjsciowy(calc_ag_kanal)
         End If

         'ustawiam nowy speed
         If Calc_ag_sect = 1 Then Speed(calc_ag_kanal) = Section1_down_speed(calc_ag_kanal)
         If Calc_ag_sect = 2 Then Speed(calc_ag_kanal) = Section2_down_speed(calc_ag_kanal)
         If Calc_ag_sect = 3 Then Speed(calc_ag_kanal) = Section3_down_speed(calc_ag_kanal)
         If Calc_ag_sect = 4 Then Speed(calc_ag_kanal) = Section4_down_speed(calc_ag_kanal)
         If Calc_ag_sect = 5 Then Speed(calc_ag_kanal) = Section5_down_speed(calc_ag_kanal)


         'nie musze sprawdzac czy wzmocnienie miesci sie w maksymalnym ag_max i ratio, bo i tak dekrementuje
         If Ag_gain(calc_ag_kanal) >= 1 Then
            Ag_gain(calc_ag_kanal) = Ag_gain(calc_ag_kanal) - 1
         Else
            Ag_gain(calc_ag_kanal) = 1
         End If
      End If

      'jesli poziom wejsciowy jest rowny docelowemu
      If Lvl_wyjsciowy(calc_ag_kanal) = Lvl_docelowy(calc_ag_kanal) Then
          'tu nie podejmuje zadnych dzialan
         If Calc_ag_kanal = 1 Then
            Parametr(17) = Lvl_wyjsciowy(calc_ag_kanal)
         End If
         If Calc_ag_kanal = 2 Then
            Parametr(18) = Lvl_wyjsciowy(calc_ag_kanal)
         End If


      End If


   End If


End Sub



'wyznaczanie w ktorej sekcji znajduje sie poziom sygnalu
Function Lvl2sect(byval Dim Lvl2sect_lvl As Byte , Byval Dim Lvl2sect_dest As Byte , Byval Dim Lvl2sect_kanal As Byte)

   If Lvl2sect_lvl > 84 Then Lvl2sect_lvl = 84              'ograniczenie wyniki od gory aby uniknac bledu odejmowania
   If Lvl2sect_dest > 84 Then Lvl2sect_dest = 84            'ograniczenie wyniki od gory aby uniknac bledu odejmowania
   If Lvl2sect_lvl < 32 Then Lvl2sect_lvl = 32
   If Lvl2sect_dest < 32 Then Lvl2sect_dest = 32

   Dim Lvl2sect_a As Byte
   Dim Lvl2sect_b As Byte
   Lvl2sect_a = Lvl2sect_lvl + Lvl2sect_dest
   Lvl2sect_a = Lvl2sect_a - 63

   Dim Lvl2sect_c As Byte
   Lvl2sect_c = 1

   If Lvl2sect_kanal = 1 Then
      For Lvl2sect_b = 2 To 5
         If Lvl2sect_a >= Kanal1_section_lvl(lvl2sect_b -1) And Lvl2sect_a < Kanal1_section_lvl(lvl2sect_b) Then Lvl2sect_c = Lvl2sect_b
      Next Lvl2sect_b
   End If

   If Lvl2sect_kanal = 2 Then
      For Lvl2sect_b = 2 To 5
         If Lvl2sect_a >= Kanal2_section_lvl(lvl2sect_b -1) And Lvl2sect_a < Kanal2_section_lvl(lvl2sect_b) Then Lvl2sect_c = Lvl2sect_b
      Next Lvl2sect_b
   End If

   Lvl2sect = Lvl2sect_c

End Function

'obliczanie wspolczynnika ratio
Function Ratio_gain(ratio_gain_wejsciowy , Ratio_gain_docelowy , Ratio_gain_ratio)
   Dim Ratio_gain_a As Byte
   Dim Ratio_gain_b As Byte
   Dim Ratio_gain_c As Byte
   Dim Ratio_gain_d As Byte

   Ratio_gain_d = Ratio_gain_ratio / 2

   ' (docelowy-wejsciowy)/ratio
   Ratio_gain_a = Ratio_gain_docelowy * 2

   Ratio_gain_a = Ratio_gain_a + Ratio_gain_d
   Ratio_gain_a = Ratio_gain_a / Ratio_gain_ratio
   Ratio_gain_b = Ratio_gain_wejsciowy * 2

   Ratio_gain_b = Ratio_gain_b + Ratio_gain_d
   Ratio_gain_b = Ratio_gain_b / Ratio_gain_ratio
   Ratio_gain_c = 63 + Ratio_gain_a
   Ratio_gain_c = Ratio_gain_c - Ratio_gain_b
   Ratio_gain = Ratio_gain_c
End Function


'pomiar wejsc ADC i skonwertowanie od razu na dB
Function Pomiaradc(byval Adc_pomiar_kanal As Byte)
   Dim Adc_lin As Word                                      'wynik pomiaru bezposrednio z ADC

   Adc_lin = Getadc(adc_pomiar_kanal)
   Adc_lin = Getadc(adc_pomiar_kanal)                       'to moze bedzie dalo sie usunac, poki co dla zwiekszenia stabilnosci pomiaru

   Pomiaradc = Adc_lin
   Pomiaradc = Adc2db(adc_lin)
End Function


'funkcja sumujaca wartosci wyrazane w dB
Function Add_db(add_db_a , Add_db_b)
   Dim Add_db_c As Byte
   Add_db_c = Add_db_a
   Add_db = 0
   'A powinno byc wieksze lub rowne B
   If Add_db_a < Add_db_b Then
      Add_db_a = Add_db_b
      Add_db_b = Add_db_c
   End If

   Add_db_c = Add_db_a - Add_db_b                           'roznica

   If Add_db_c >= 10 Then Add_db = Add_db_a
   If Add_db_c < 10 And Add_db_c > 4 Then Add_db = Add_db_a + 1
   If Add_db_c <= 4 And Add_db_c > 1 Then Add_db = Add_db_a + 2
   If Add_db_c <= 1 Then Add_db = Add_db_a + 3
End Function


'konwersja wartosci liniowej z ADC na dB
Function Adc2db(adc2db_dbl)
  Dim Adc2db_b As Byte
  Dim Adc2db_c As Byte
  Dim Adc2db_db_tab(42) As Word
  Adc2db_b = 0
  Adc2db_c = 0

  Adc2db_db_tab(1) = 3
  Adc2db_db_tab(2) = 6                                      '-30db
  Adc2db_db_tab(3) = 9
  Adc2db_db_tab(4) = 11
  Adc2db_db_tab(5) = 14
  Adc2db_db_tab(6) = 18
  Adc2db_db_tab(7) = 22
  Adc2db_db_tab(8) = 26
  Adc2db_db_tab(9) = 31
  Adc2db_db_tab(10) = 37
  Adc2db_db_tab(11) = 43
  Adc2db_db_tab(12) = 50                                    '-20db
  Adc2db_db_tab(13) = 58
  Adc2db_db_tab(14) = 67
  Adc2db_db_tab(15) = 77
  Adc2db_db_tab(16) = 88
  Adc2db_db_tab(17) = 101
  Adc2db_db_tab(18) = 116
  Adc2db_db_tab(19) = 132
  Adc2db_db_tab(20) = 150
  Adc2db_db_tab(21) = 170
  Adc2db_db_tab(22) = 192                                   '-10db
  Adc2db_db_tab(23) = 218
  Adc2db_db_tab(24) = 246
  Adc2db_db_tab(25) = 277
  Adc2db_db_tab(26) = 313
  Adc2db_db_tab(27) = 353
  Adc2db_db_tab(28) = 398
  Adc2db_db_tab(29) = 449                                   '-3db
  Adc2db_db_tab(30) = 505                                   '-2db
  Adc2db_db_tab(31) = 567                                   '-1db
  Adc2db_db_tab(32) = 636                                   '0db
  Adc2db_db_tab(33) = 712
  Adc2db_db_tab(34) = 799
  Adc2db_db_tab(35) = 897                                   '+3db
  Adc2db_db_tab(36) = 1006
  Adc2db_db_tab(37) = 1128
  Adc2db_db_tab(38) = 1266                                  '+6db
  Adc2db_db_tab(39) = 1421
  Adc2db_db_tab(40) = 1594
  Adc2db_db_tab(41) = 1788                                  '+9db
  Adc2db_db_tab(42) = 65535

  'dziele skan na pol i od razu przenosze do wlasciwej polowki dla optymalizacji szybkosci kodu
  If Adc2db_dbl >= Adc2db_db_tab(22) Then
    For Adc2db_b = 22 To 42
       If Adc2db_dbl >= Adc2db_db_tab(adc2db_b) And Adc2db_dbl < Adc2db_db_tab(adc2db_b + 1) Then Adc2db_c = Adc2db_b
    Next Adc2db_b
  Else
     For Adc2db_b = 1 To 21
        If Adc2db_dbl >= Adc2db_db_tab(adc2db_b) And Adc2db_dbl < Adc2db_db_tab(adc2db_b + 1) Then Adc2db_c = Adc2db_b
     Next Adc2db_b
  End If


  Adc2db = Adc2db_c + 31
End Function


'odczyt z pamieci RAM (ktora strona; poczatkowy adres; ilosc bajtow)
Sub Ram_page_read(ram_page_read_param As Byte , Ram_page_read_adr As Byte , Ram_page_read_ile)

   'konfiguracja pinow
   Config Ram_cs = Output
   Config Ram_clk = Output
   Config Ram_sdi = Output
   Config Ram_sdo = Input


'port d.6 wejscie w uC na dane
   Dim Ram_page_read_loop As Byte
   Dim Ram_page_read_param_first_adr As Byte
   Dim Ram_page_read_param_last_adr As Byte


   Dim Ram_page_read_wyslac As Byte
   Ram_page_read_param_first_adr = Ram_page_read_adr

   Ram_page_read_param_last_adr = Ram_page_read_param_first_adr + Ram_page_read_ile
   Ram_page_read_param_last_adr = Ram_page_read_param_last_adr - 1

   Dim Ram_page_read_odebrane As Byte

   Dim Ram_page_write_param As Byte
   Ram_page_write_param = Ram_page_read_param

    Ram_cs = 0


    'zabezpieczanie rejestrow do pamieci sram
    !STS {Ram_r21}, R21
    !STS {Ram_r23}, R23

   'nadawanie komendy read
         !LDI R21, 3                                        'wrzucam do R21 dane do wyslania
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_page_read1:
            !CBI PORTD, 7                                   'clk=0

            !SBRS R21,7                                     'sprawdz czy 1 w bicie 7 i jesli tak, to pomin nastepne
            !Cbi PORTD, 5                                   'ustaw 0 na sdo
            !SBRC R21,7                                     'sprawdz czy 0 w bicie  7 i jesli tak, to pomin nastepne
            !SBI PORTD, 5                                   'ustaw 1 na sdo

            !NOP
            !SBI PORTD, 7                                   'clk=1

            !LSL R21                                        'przesun rejestr w lewo

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_page_read1                           'powrot na poczatek petli
            !nop
            !CBI PORTD, 7                                   'clk=0
            !nop


   'nadawanie adresu poczatku strony (16-bit, starsza czesc) (00000000, od 0)
         Ram_page_read_wyslac = 0
         !LDS R21, {ram_page_read_wyslac}                   'wrzucam do R21 dane do wyslania
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_page_read2:
            !CBI PORTD, 7                                   'clk=0

            !SBRS R21,7                                     'sprawdz czy 1 w bicie 7 i jesli tak, to pomin nastepne
            !Cbi PORTD, 5                                   'ustaw 0 na sdo
            !SBRC R21,7                                     'sprawdz czy 0 w bicie  7 i jesli tak, to pomin nastepne
            !SBI PORTD, 5                                   'ustaw 1 na sdo

            !NOP
            !SBI PORTD, 7                                   'clk=1

            !LSL R21                                        'przesun rejestr w lewo

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_page_read2                           'powrot na poczatek petli
            !nop
            !CBI PORTD, 7                                   'clk=0
            !nop


   'nadawanie adresu poczatku strony (16-bit, mlodsza czesc)
         Ram_page_read_wyslac = Ram_page_read_adr
         !LDS R21, {ram_page_read_wyslac}                   'wrzucam do R21 dane do wyslania
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_page_read3:
            !CBI PORTD, 7                                   'clk=0

            !SBRS R21,7                                     'sprawdz czy 1 w bicie 7 i jesli tak, to pomin nastepne
            !Cbi PORTD, 5                                   'ustaw 0 na sdo
            !SBRC R21,7                                     'sprawdz czy 0 w bicie  7 i jesli tak, to pomin nastepne
            !SBI PORTD, 5                                   'ustaw 1 na sdo

            !NOP
            !SBI PORTD, 7                                   'clk=1

            !LSL R21                                        'przesun rejestr w lewo

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_page_read3                           'powrot na poczatek petli
            !nop
            !CBI PORTD, 7                                   'clk=0
            !nop


   'odbieranie danych
   For Ram_page_read_loop = Ram_page_read_param_first_adr To Ram_page_read_param_last_adr

         !clr R21                                           'zeruje R21 - przygotowuje do odbioru
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_page_read4:
            !SBI PORTD, 7                                   'clk=1

            !LSL R21                                        'przesun rejestr w lewo
            !ANDI R21, 254                                  'ustaw ostatni bit z prawej na 0 (R23 AND 11111110)


            !SBIC PIND, PIND6                               ' sprawdzam czy na PD6 jest 1
            !ORI R21, 1                                     'tu nalezy ustawic ostatni bit z prawej na 1 (R23 OR 00000001)
'            !NOP

            !CBI PORTD, 7                                   'clk=0

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_page_read4                           'powrot na poczatek petli
'            !nop
            !CBI PORTD, 7                                   'clk=0

            !nop

            'wrzucanie do parametru zawartosci rejestru R21
            !STS {Ram_page_read_odebrane}, R21

            Parametr_odczyt(ram_page_write_param) = Ram_page_read_odebrane

            Ram_page_write_param = Ram_page_write_param + 1

      Next Ram_page_read_loop


   'przywracanie rejestrow z pamieci sram
    !LDS R21, {Ram_r21}
    !LDS R23, {Ram_r23}

      Ram_cs = 1

   'konfiguracja pinow po transmisji
   Config Ram_cs = Input
   Config Ram_clk = Input
   Config Ram_sdi = Input
   Config Ram_sdo = Input

End Sub

'zapis do pamieci RAM (ktora strona; poczatkowy adres; ilosc bajtow)
Sub Ram_page_write(ram_page_write_param As Byte , Ram_page_write_adr As Byte , Ram_page_write_ile As Byte)

   'konfiguracja pinow
   Config Ram_cs = Output
   Config Ram_clk = Output
   Config Ram_sdi = Output
   Config Ram_sdo = Input


   Dim Ram_page_write_loop As Byte
   Dim Ram_page_write_param_first_adr As Byte
   Dim Ram_page_write_param_last_adr As Byte

   Dim Ram_page_write_wyslac As Byte
   Ram_page_write_param_first_adr = Ram_page_write_adr

   Ram_page_write_param_last_adr = Ram_page_write_adr + Ram_page_write_ile
   Ram_page_write_param_last_adr = Ram_page_write_param_last_adr - 1
   Dim Ram_page_write_param_adr As Byte

   Ram_page_write_param_adr = Ram_page_write_param

    Ram_cs = 0

    'zabezpieczanie rejestrow do pamieci sram
    !STS {Ram_r21}, R21
    !STS {Ram_r23}, R23

   'nadawanie komendy write
         !LDI R21, 2                                        'wrzucam do R21 komende write
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_page_write1:
            !CBI PORTD, 7                                   'clk=0

            !SBRS R21,7                                     'sprawdz czy 1 w bicie 7 i jesli tak, to pomin nastepne
            !Cbi PORTD, 5                                   'ustaw 0 na sdo
            !SBRC R21,7                                     'sprawdz czy 0 w bicie  7 i jesli tak, to pomin nastepne
            !SBI PORTD, 5                                   'ustaw 1 na sdo

            !NOP
            !SBI PORTD, 7                                   'clk=1

            !LSL R21                                        'przesun rejestr w lewo

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_page_write1                          'powrot na poczatek petli
            !nop
            !CBI PORTD, 7                                   'clk=0
            !nop


   'nadawanie adresu poczatku strony (16-bit starsza czesc)  (00000000 od 0)
         Ram_page_write_wyslac = 0
         !LDS R21, {Ram_page_write_wyslac}                  'wrzucam do R21 pierwszy adres
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_page_write2:
            !CBI PORTD, 7                                   'clk=0

            !SBRS R21,7                                     'sprawdz czy 1 w bicie 7 i jesli tak, to pomin nastepne
            !Cbi PORTD, 5                                   'ustaw 0 na sdo
            !SBRC R21,7                                     'sprawdz czy 0 w bicie  7 i jesli tak, to pomin nastepne
            !SBI PORTD, 5                                   'ustaw 1 na sdo

            !NOP
            !SBI PORTD, 7                                   'clk=1

            !LSL R21                                        'przesun rejestr w lewo

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_page_write2                          'powrot na poczatek petli
            !nop
            !CBI PORTD, 7                                   'clk=0
            !nop



   'nadawanie adresu poczatku strony (16-bit mlodsza czesc)
         Ram_page_write_wyslac = Ram_page_write_adr
         !LDS R21, {Ram_page_write_wyslac}                  'wrzucam do R21 pierwszy adres
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_page_write3:
            !CBI PORTD, 7                                   'clk=0

            !SBRS R21,7                                     'sprawdz czy 1 w bicie 7 i jesli tak, to pomin nastepne
            !Cbi PORTD, 5                                   'ustaw 0 na sdo
            !SBRC R21,7                                     'sprawdz czy 0 w bicie  7 i jesli tak, to pomin nastepne
            !SBI PORTD, 5                                   'ustaw 1 na sdo

            !NOP
            !SBI PORTD, 7                                   'clk=1

            !LSL R21                                        'przesun rejestr w lewo

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_page_write3                          'powrot na poczatek petli
            !nop
            !CBI PORTD, 7                                   'clk=0
            !nop


   'nadawanie danych
   For Ram_page_write_loop = Ram_page_write_param_first_adr To Ram_page_write_param_last_adr


      Ram_page_write_wyslac = Parametr(ram_page_write_param)

         !LDS R21, {ram_page_write_wyslac}                  'wrzucam do R21 dane do wyslania
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_page_write4:
            !CBI PORTD, 7                                   'clk=0

            !SBRS R21,7                                     'sprawdz czy 1 w bicie 7 i jesli tak, to pomin nastepne
            !Cbi PORTD, 5                                   'ustaw 0 na sdo
            !SBRC R21,7                                     'sprawdz czy 0 w bicie  7 i jesli tak, to pomin nastepne
            !SBI PORTD, 5                                   'ustaw 1 na sdo

            !NOP
            !SBI PORTD, 7                                   'clk=1

            !LSL R21                                        'przesun rejestr w lewo

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_page_write4                          'powrot na poczatek petli
            !nop
            !CBI PORTD, 7                                   'clk=0

            !nop

            Ram_page_write_param = Ram_page_write_param + 1
      Next Ram_page_write_loop


   'przywracanie rejestrow z pamieci sram
    !LDS R21, {Ram_r21}
    !LDS R23, {Ram_r23}

      Ram_cs = 1

   'konfiguracja pinow po transmisji
   Config Ram_cs = Input
   Config Ram_clk = Input
   Config Ram_sdi = Input
   Config Ram_sdo = Input


End Sub

'nadawanie do PGA2310
'192 = 0dB
Sub Pga(pga_r As Byte , Pga_l As Byte)

   Dim Pga_i As Byte
   Dim Pga_wy As Byte
   Dim Pga_wyslac As Byte

   'konwersja na format PGA
   Pga_r = Pga_r * 2
   Pga_r = Pga_r + 66
   Pga_l = Pga_l * 2
   Pga_l = Pga_l + 66

   'konfiguracja pinow
   Dcaf_dat = 0
   Dcaf_clk = 0
   Dcaf_cs = 0

   'pierwsze 8 bitow
   Pga_wyslac = Pga_r
   For Pga_i = 0 To 7
      Pga_wy = Pga_wyslac And &B10000000
      Shift Pga_wy , Right , 7
      Dcaf_dat = Pga_wy
      Waitus 2
      Dcaf_clk = 1
      Waitus 3
      Dcaf_clk = 0
      Shift Pga_wyslac , Left , 1
   Next Pga_i

   'drugie 8 bitow
   Pga_wyslac = Pga_l
   For Pga_i = 0 To 7
      Pga_wy = Pga_wyslac And &B10000000
      Shift Pga_wy , Right , 7
      Dcaf_dat = Pga_wy
      Waitus 2
      Dcaf_clk = 1
      Waitus 3
      Dcaf_clk = 0
      Shift Pga_wyslac , Left , 1
   Next Pga_i

   Waitus 1
   Dcaf_cs = 1
   Dcaf_dat = 0

End Sub


'nadawanie do CD4094
Sub Eq(eq_dat1 As Byte , Eq_dat2 As Byte , Eq_dat3 As Byte , Eq_dat4 As Byte)



   Dim Eq_i As Byte
   Dim Eq_wy As Byte
   Dim Eq_wyslac As Byte

   'konfiguracja pinow
   Dcaf_dat = 0
   Dcaf_str = 0
   Dcaf_clk = 0

   'pierwsze 8 bitow
   Eq_wyslac = Eq_dat1 * 16
   Eq_wyslac = Eq_wyslac + Eq_dat2
   For Eq_i = 0 To 7
      Eq_wy = Eq_wyslac And &B10000000
      Shift Eq_wy , Right , 7
      Dcaf_dat = Eq_wy
      Waitus 2
      Dcaf_clk = 1
      Waitus 3
      Dcaf_clk = 0
      Shift Eq_wyslac , Left , 1
   Next Eq_i

   'drugie 8 bitow
   Eq_wyslac = Eq_dat3 * 16
   Eq_wyslac = Eq_wyslac + Eq_dat4
   For Eq_i = 0 To 7
      Eq_wy = Eq_wyslac And &B10000000
      Shift Eq_wy , Right , 7
      Dcaf_dat = Eq_wy
      Waitus 2
      Dcaf_clk = 1
      Waitus 3
      Dcaf_clk = 0
      Shift Eq_wyslac , Left , 1
   Next Eq_i


   Dcaf_str = 1
   Waitus 1
   Dcaf_str = 0
   Dcaf_dat = 0

End Sub