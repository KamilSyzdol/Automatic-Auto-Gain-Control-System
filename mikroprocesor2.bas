'28.01.2024
'uC nr 2

$regfile = "m328def.dat"                                    ' specify the used micro
$crystal = 8192000                                          ' used crystal frequency
$hwstack = 32                                               ' default use 32 for the hardware stack
$swstack = 10                                               ' default use 10 for the SW stack
$framesize = 40                                             ' default use 40 for the frame space

$baud = 256000


Waitms 50

'tablica parametrow odczytanych z RAM
Dim Parametr_odczyt_ram(40) As Byte

Declare Sub Ram_page_write(byval Ram_page_write_param As Byte , Byval Ram_page_write_adr As Byte , Byval Ram_page_write_ile As Byte)
Declare Sub Ram_page_read(byval Ram_page_read_param As Byte , Byval Ram_page_read_adr As Byte , Byval Ram_page_read_ile As Byte , Byval Ram_page_read_bank As Byte)

Declare Sub Ram_write_sreg()
Declare Sub Ram_read_sreg()

Declare Sub Ram_lcd_write()


Declare Sub Set_cs_adr(byval Cs_adr As Byte)

Declare Sub Tc_write(byval Tc_left As Byte , Byval Tc_right As Byte , Byval Tc_adr As Byte)
Declare Sub Przesuwny_write(byval Lim_mux As Byte , Byval Bypass_pk As Byte)
Declare Sub Wms_write(byval Wms_adr As Byte , Byval Wms_cmd1 As Byte , Byval Wms_dat1 As Byte , Byval Wms_cmd2 As Byte , Byval Wms_dat2 As Byte , Byval Wms_cmd3 As Byte , Byval Wms_dat3 As Byte )

Declare Function Thres2wms(byval Threswms As Byte) As Byte
Declare Function Ratio2wms(byval Ratiowms As Byte) As Byte
Declare Function Attack2wms(byval Attackwms As Byte) As Byte
Declare Function Release2wms(byval Releasewms As Byte) As Byte



'zmienne przechowujace rejestry R23 i R21 na czas wstawek assemblerowych
Dim Ram_r23 As Byte
Dim Ram_r21 As Byte


'sterowanie RAM
Ram_cs Alias Portb.0
Ram_clk Alias Portd.7
Ram_sdo Alias Portd.6                                       'podlaczyc do SDO pamieci
Ram_sdi Alias Portd.5                                       'podlaczyc do SDI pamieci

'konfiguracja pinow pamieci RAM
Config Ram_cs = Input
Config Ram_clk = Input
Config Ram_sdi = Input
Config Ram_sdo = Input

'token ring
Busy_out Alias Portb.1
Config Busy_out = Output


'konfiguracja pinow CS adresowanych
Cs_a Alias Portb.2
Cs_b Alias Portb.3
Cs_c Alias Portb.4
Cs_d Alias Portb.5
Cs Alias Portc.2

Config Cs_a = Output
Config Cs_b = Output
Config Cs_c = Output
Config Cs_d = Output
Config Cs = Output


'klawiatura
Dim Kbd_read_table(4) As Byte
Declare Sub Kbd_read()
Dim Kbd_odebrane As Byte

'sterowanie TM1638
Tm_clk Alias Portc.4
Tm_sdio Alias Portc.3                                       'podlaczyc do SDIO ukladu
Config Tm_clk = Input
Config Tm_sdio = Input


'magistrala sterujaca
Clk_bus Alias Portc.0
Dat_bus Alias Portc.1
Config Clk_bus = Output
Config Dat_bus = Output


Dim Wskaznik As Byte
Dim Menu As Byte

Dim Tmp_str As String * 1
Dim Tmp_byte As Byte

 Dim Ram_i As Byte
 Ram_i = 0

'pozycja startowa
Menu = 1
Wskaznik = 1

'przytrzymanie tytulu menu
Dim Timer_wewnetrzny1 As Byte
Timer_wewnetrzny1 = 0

'blokada klawiatury na okreslony czas
Dim Timer_wewnetrzny2 As Byte
Timer_wewnetrzny1 = 0


Dim Wierszx As String * 40


'zmienne przechowujace caly wiersz
Declare Sub Oblicz_wiersz1(byval Menu As Byte)
Declare Sub Oblicz_wiersz2(byval Menu As Byte)
Declare Sub Oblicz_wiersz3(byval Menu As Byte , Byval Wskaznik As Byte)



'przerwanie z int0
Config Portd.2 = Input
Portd.2 = 1

Enable Int0
Config Int0 = Falling
Enable Interrupts
On Int0 Poczatek

'flaga przerwania
Dim Przerwanie_flag As Byte
'ustawiam 1 zeby pierwszy raz moglo przejsc bez wyzwolenia od AGC
'tu zaczyna sie token-ring po uruchomieniu urzadzenia
Przerwanie_flag = 1


'przerwanie z int1 (enkoder)
Config Portd.3 = Input
Config Portd.4 = Input
Portd.3 = 1
Portd.4 = 1

Dim Enkoder_stan As Byte
Enable Int1
Config Int1 = Falling
Enable Interrupts
On Int1 Przerwanie_enkoder

Dim Param_enkoder As Byte
Dim Param_kbd As Byte
Dim Kbd_val As Byte


'flaga zmiany parametrow = koniecznosc wyslania na WMS, TC i 4094
Dim Zmiana_flag As Byte
'pierwsza petla konfiguruje wszystkie uklady, dlatego postawiona flaga
Zmiana_flag = 1



'deklaracja pol w trzecim wierszu
Dim Menu_field1 As String * 6 : Dim Menu_field2 As String * 6 : Dim Menu_field3 As String * 6 : Dim Menu_field4 As String * 6 : Dim Menu_field5 As String * 6


'wstepne zaprogramowanie parametrow (podstawowy preset)
Dim Parametr(130) As Byte
Parametr(1) = 0 : Parametr(2) = 0 : Parametr(3) = 15 : Parametr(4) = 25 : Parametr(5) = 0
Parametr(6) = 0 : Parametr(7) = 0 : Parametr(8) = 15 : Parametr(9) = 25 : Parametr(10) = 0
Parametr(11) = 4 : Parametr(12) = 11 : Parametr(13) = 12 : Parametr(14) = 14 : Parametr(15) = 18
Parametr(16) = 5 : Parametr(17) = 7 : Parametr(18) = 9 : Parametr(19) = 11 : Parametr(20) = 11
Parametr(21) = 2 : Parametr(22) = 5 : Parametr(23) = 7 : Parametr(24) = 5 : Parametr(25) = 3
Parametr(26) = 13 : Parametr(27) = 11 : Parametr(28) = 9 : Parametr(29) = 7 : Parametr(30) = 4
Parametr(31) = 0 : Parametr(32) = 6 : Parametr(33) = 11 : Parametr(34) = 13 : Parametr(35) = 36
Parametr(36) = 0 : Parametr(37) = 6 : Parametr(38) = 11 : Parametr(39) = 13 : Parametr(40) = 36
Parametr(41) = 0 : Parametr(42) = 0 : Parametr(43) = 0 : Parametr(44) = 0 : Parametr(45) = 0
Parametr(46) = 0 : Parametr(47) = 0 : Parametr(48) = 0 : Parametr(49) = 0 : Parametr(50) = 0

Parametr(51) = 0 : Parametr(52) = 15 : Parametr(53) = 0 : Parametr(54) = 15 : Parametr(55) = 0
Parametr(56) = 3 : Parametr(57) = 2 : Parametr(58) = 3 : Parametr(59) = 5 : Parametr(60) = 10
Parametr(61) = 3 : Parametr(62) = 2 : Parametr(63) = 3 : Parametr(64) = 5 : Parametr(65) = 10
Parametr(66) = 3 : Parametr(67) = 2 : Parametr(68) = 3 : Parametr(69) = 5 : Parametr(70) = 10
Parametr(71) = 3 : Parametr(72) = 2 : Parametr(73) = 3 : Parametr(74) = 5 : Parametr(75) = 10
Parametr(76) = 3 : Parametr(77) = 2 : Parametr(78) = 3 : Parametr(79) = 5 : Parametr(80) = 10
Parametr(81) = 3 : Parametr(82) = 2 : Parametr(83) = 3 : Parametr(84) = 5 : Parametr(85) = 10
Parametr(86) = 1 : Parametr(87) = 1 : Parametr(88) = 1 : Parametr(89) = 0 : Parametr(90) = 0
Parametr(91) = 1 : Parametr(92) = 1 : Parametr(93) = 1 : Parametr(94) = 0 : Parametr(95) = 0
Parametr(96) = 1 : Parametr(97) = 1 : Parametr(98) = 1 : Parametr(99) = 1 : Parametr(100) = 10

Parametr(101) = 1 : Parametr(102) = 1 : Parametr(103) = 0 : Parametr(104) = 0 : Parametr(105) = 10
Parametr(106) = 1 : Parametr(107) = 0 : Parametr(108) = 0 : Parametr(109) = 0 : Parametr(110) = 0
Parametr(111) = 1 : Parametr(112) = 1 : Parametr(113) = 1 : Parametr(114) = 0 : Parametr(115) = 1
Parametr(116) = 0 : Parametr(117) = 0 : Parametr(118) = 0 : Parametr(119) = 0 : Parametr(120) = 0
Parametr(121) = 0 : Parametr(122) = 0 : Parametr(123) = 0 : Parametr(124) = 0 : Parametr(125) = 0
Parametr(126) = 0 : Parametr(127) = 0 : Parametr(128) = 0 : Parametr(129) = 0 : Parametr(130) = 0

'maksymalne wartosci jakie moga przyjac parametry
Dim Param_max_value(130) As Byte
Param_max_value(1) = 4 : Param_max_value(2) = 36 : Param_max_value(3) = 21 : Param_max_value(4) = 25 : Param_max_value(5) = 0
Param_max_value(6) = 4 : Param_max_value(7) = 36 : Param_max_value(8) = 21 : Param_max_value(9) = 25 : Param_max_value(10) = 0
Param_max_value(11) = 18 : Param_max_value(12) = 18 : Param_max_value(13) = 18 : Param_max_value(14) = 18 : Param_max_value(15) = 18
Param_max_value(16) = 18 : Param_max_value(17) = 18 : Param_max_value(18) = 18 : Param_max_value(19) = 18 : Param_max_value(20) = 18
Param_max_value(21) = 18 : Param_max_value(22) = 18 : Param_max_value(23) = 18 : Param_max_value(24) = 18 : Param_max_value(25) = 18
Param_max_value(26) = 18 : Param_max_value(27) = 18 : Param_max_value(28) = 18 : Param_max_value(29) = 18 : Param_max_value(30) = 18
Param_max_value(31) = 36 : Param_max_value(32) = 36 : Param_max_value(33) = 36 : Param_max_value(34) = 36 : Param_max_value(35) = 36
Param_max_value(36) = 36 : Param_max_value(37) = 36 : Param_max_value(38) = 36 : Param_max_value(39) = 36 : Param_max_value(40) = 36
Param_max_value(41) = 4 : Param_max_value(42) = 4 : Param_max_value(43) = 4 : Param_max_value(44) = 4 : Param_max_value(45) = 4
Param_max_value(46) = 4 : Param_max_value(47) = 4 : Param_max_value(48) = 4 : Param_max_value(49) = 4 : Param_max_value(50) = 4

Param_max_value(51) = 15 : Param_max_value(52) = 15 : Param_max_value(53) = 15 : Param_max_value(54) = 15 : Param_max_value(55) = 0
Param_max_value(56) = 20 : Param_max_value(57) = 10 : Param_max_value(58) = 9 : Param_max_value(59) = 9 : Param_max_value(60) = 16       'str 12
Param_max_value(61) = 20 : Param_max_value(62) = 10 : Param_max_value(63) = 9 : Param_max_value(64) = 9 : Param_max_value(65) = 16
Param_max_value(66) = 20 : Param_max_value(67) = 10 : Param_max_value(68) = 9 : Param_max_value(69) = 9 : Param_max_value(70) = 16
Param_max_value(71) = 20 : Param_max_value(72) = 10 : Param_max_value(73) = 9 : Param_max_value(74) = 9 : Param_max_value(75) = 16
Param_max_value(76) = 20 : Param_max_value(77) = 10 : Param_max_value(78) = 9 : Param_max_value(79) = 9 : Param_max_value(80) = 16
Param_max_value(81) = 20 : Param_max_value(82) = 10 : Param_max_value(83) = 9 : Param_max_value(84) = 9 : Param_max_value(85) = 16       'str 17
Param_max_value(86) = 2 : Param_max_value(87) = 2 : Param_max_value(88) = 2 : Param_max_value(89) = 0 : Param_max_value(90) = 0
Param_max_value(91) = 2 : Param_max_value(92) = 2 : Param_max_value(93) = 2 : Param_max_value(94) = 0 : Param_max_value(95) = 0
Param_max_value(96) = 20 : Param_max_value(97) = 10 : Param_max_value(98) = 9 : Param_max_value(99) = 9 : Param_max_value(100) = 16

Param_max_value(101) = 20 : Param_max_value(102) = 10 : Param_max_value(103) = 9 : Param_max_value(104) = 9 : Param_max_value(105) = 16
Param_max_value(106) = 2 : Param_max_value(107) = 0 : Param_max_value(108) = 0 : Param_max_value(109) = 0 : Param_max_value(110) = 0
Param_max_value(111) = 1 : Param_max_value(112) = 1 : Param_max_value(113) = 1 : Param_max_value(114) = 0 : Param_max_value(115) = 1
Param_max_value(116) = 0 : Param_max_value(117) = 0 : Param_max_value(118) = 0 : Param_max_value(119) = 0 : Param_max_value(120) = 0
Param_max_value(121) = 0 : Param_max_value(122) = 0 : Param_max_value(123) = 0 : Param_max_value(124) = 0 : Param_max_value(125) = 0
Param_max_value(126) = 0 : Param_max_value(127) = 0 : Param_max_value(128) = 0 : Param_max_value(129) = 0 : Param_max_value(130) = 0


Dim J As Byte
Dim I As Byte

Dim Lcd_table(80) As Byte

'zmienna testowa do testowania menu
Dim Tmp_x As Byte


'glowna petla
Do

      'jesli nastapilo przerwanie z INT0(lub na start) (od uC1)
      If Przerwanie_flag = 1 Then

         'znak poczatku pracy - diagnostyczny
         Call Set_cs_adr(10)
         Cs = 0
         Waitus 200
         Cs = 1
         Call Set_cs_adr(0)


         Busy_out = 1                                       'token ring

         'nadawanie danych do RAM dla uC AGC
         Call Ram_page_write(1 , 1 , 130)
         'nadawanie danych do RAM dla uC LCD
         Call Ram_lcd_write()



         Dim Temp_i As Byte
   'wysylanie na uart danych z wyswietlacza
   'co drugi przelot wysylam na zmiane gore i dol wyswietlacza
   If Ram_i = 1 Or Ram_i = 3 Or Ram_i = 5 Then

         Call Ram_page_read(1 , 0 , 40 , 1)

         Print "LCD1!";
            For Temp_i = 1 To 40
               Print Chr(parametr_odczyt_ram(temp_i));
            Next Temp_i
         Print "!" ; Chr(13) ; Chr(10) ;


   Else

         Call Ram_page_read(1 , 40 , 40 , 1)

         Print "LCD2!";
            For Temp_i = 1 To 40
               Print Chr(parametr_odczyt_ram(temp_i));
            Next Temp_i
         Print "!" ; Chr(13) ; Chr(10) ;

   End If




      Dim Temp_j As Byte

         Ram_i = Ram_i + 1
         If Ram_i >= 5 Then Ram_i = 1

            Temp_j = Ram_i * 40
            Temp_j = Temp_j - 40

            Call Ram_page_read(1 , Temp_j , 40 , 0)
            Print "Da0" ; Ram_i ; "!";
            For Temp_i = 1 To 40
               Print "@" ; Chr(parametr_odczyt_ram(temp_i));
            Next Temp_i
            Print "@" ; "!" ; Chr(13) ; Chr(10);


         'z kazdym cyklem beda wysylaly sie parametry ADC i AGC
            Temp_j = 5 * 40
            Temp_j = Temp_j - 40

            Call Ram_page_read(1 , Temp_j , 40 , 0)
            Print "Da0" ; 5 ; "!";
      '      Print "Da01!";
            For Temp_i = 1 To 40
               Print "@" ; Chr(parametr_odczyt_ram(temp_i));
            Next Temp_i
            Print "@" ; "!" ; Chr(13) ; Chr(10);


         Busy_out = 0                                       'token ring


      If Timer_wewnetrzny2 >= 4 Then

         Call Kbd_read()

         'nawigowanie po menu
         If Kbd_read_table(4) = 64 Then
            Menu = Menu - 1
            'ukryte menu 18 i 19, poniewaz nie powstala plytka multipleksera kompresora
            If Menu = 18 Then Menu = 17
            If Menu = 19 Then Menu = 17
            'tez ukryte, mozna zagospodarowac w przyszlosci
            If Menu = 24 Then Menu = 23
            Timer_wewnetrzny2 = 0
            Timer_wewnetrzny1 = 0
         End If

         If Kbd_read_table(2) = 2 Then
            Menu = Menu + 1
            'ukryte menu 18 i 19, poniewaz nie powstala plytka multipleksera kompresora
            If Menu = 18 Then Menu = 20
            If Menu = 19 Then Menu = 20
            'tez ukryte, mozna zagospodarowac w przyszlosci
            If Menu = 24 Then Menu = 23
            Timer_wewnetrzny2 = 0
            Timer_wewnetrzny1 = 0
         End If

      If Menu <> 50 Then                                    'uwzgledniam o programie
         If Menu >= 24 Then Menu = 24
         If Menu <= 1 Then Menu = 1
      End If

         'ustawianie wskaznika na odpowiednie pole
         If Kbd_read_table(4) = 32 Then
            Wskaznik = 1
            Kbd_val = 1
            Timer_wewnetrzny2 = 0
            Zmiana_flag = 1
         End If

         If Kbd_read_table(4) = 4 Then
            Wskaznik = 1
            Kbd_val = 2
            Timer_wewnetrzny2 = 0
            Zmiana_flag = 1
         End If

         If Kbd_read_table(4) = 2 Then
            Wskaznik = 2
            Kbd_val = 1
            Timer_wewnetrzny2 = 0
            Zmiana_flag = 1
         End If

         If Kbd_read_table(3) = 64 Then
            Wskaznik = 2
            Kbd_val = 2
            Timer_wewnetrzny2 = 0
            Zmiana_flag = 1
         End If

         If Kbd_read_table(3) = 32 Then
            Wskaznik = 3
            Kbd_val = 1
            Timer_wewnetrzny2 = 0
            Zmiana_flag = 1
         End If

         If Kbd_read_table(3) = 4 Then
            Wskaznik = 3
            Kbd_val = 2
            Timer_wewnetrzny2 = 0
            Zmiana_flag = 1
         End If

         If Kbd_read_table(3) = 2 Then
            Wskaznik = 4
            Kbd_val = 1
            Timer_wewnetrzny2 = 0
            Zmiana_flag = 1
         End If

         If Kbd_read_table(2) = 64 Then
            Wskaznik = 4
            Kbd_val = 2
            Timer_wewnetrzny2 = 0
            Zmiana_flag = 1
         End If

         If Kbd_read_table(2) = 32 Then
            Wskaznik = 5
            Kbd_val = 1
            Timer_wewnetrzny2 = 0
            Zmiana_flag = 1
         End If

         If Kbd_read_table(2) = 4 Then
            Wskaznik = 5
            Kbd_val = 2
            Timer_wewnetrzny2 = 0
            Zmiana_flag = 1
         End If


      '--obsluga inkrementacji/dekr z klawiatury--------------------------------------
         Param_kbd = Menu - 1
         Param_kbd = Param_kbd * 5
         Param_kbd = Param_kbd + Wskaznik

         If Kbd_val = 1 Then
            If Parametr(param_kbd) > 0 Then Parametr(param_kbd) = Parametr(param_kbd) - 1
         End If

         If Kbd_val = 2 Then
            If Parametr(param_kbd) < Param_max_value(param_kbd) Then Parametr(param_kbd) = Parametr(param_kbd) + 1
         End If

         Kbd_val = 0


         If Kbd_read_table(4) = 68 Then
            Menu = 1                                        'skrot do AGC
            Timer_wewnetrzny2 = 0
            Timer_wewnetrzny1 = 0
         End If

         If Kbd_read_table(4) = 64 And Kbd_read_table(3) = 64 Then
            Menu = 11                                       'skrot do filtra
            Timer_wewnetrzny2 = 0
            Timer_wewnetrzny1 = 0
         End If

         If Kbd_read_table(4) = 64 And Kbd_read_table(3) = 4 Then
            Menu = 12                                       'skrot do kompresora
            Timer_wewnetrzny2 = 0
            Timer_wewnetrzny1 = 0
         End If

         If Kbd_read_table(4) = 64 And Kbd_read_table(2) = 64 Then
            Menu = 20                                       'skrot do limitera
            Timer_wewnetrzny2 = 0
            Timer_wewnetrzny1 = 0
         End If

         If Kbd_read_table(4) = 64 And Kbd_read_table(2) = 4 Then
            Menu = 23                                       'skrot do bypass
            Timer_wewnetrzny2 = 0
            Timer_wewnetrzny1 = 0
         End If

         If Kbd_read_table(4) = 64 And Kbd_read_table(3) = 64 And Kbd_read_table(2) = 2 Then
            Menu = 50                                       'o programie
            Timer_wewnetrzny2 = 0
            Timer_wewnetrzny1 = 0
         End If


      End If
      Timer_wewnetrzny2 = Timer_wewnetrzny2 + 1


      'obsluga timera do wyswietlania menu
      Timer_wewnetrzny1 = Timer_wewnetrzny1 + 1
      If Timer_wewnetrzny1 >= 40 Then Timer_wewnetrzny1 = 40

      If Timer_wewnetrzny1 < 40 Then
         Call Oblicz_wiersz1(menu)
      Else
         Call Oblicz_wiersz2(menu)
      End If

         Call Oblicz_wiersz3(menu , Wskaznik)


      '--obsluga enkodera--------------------------------------
         Param_enkoder = Menu - 1
         Param_enkoder = Param_enkoder * 5
         Param_enkoder = Param_enkoder + Wskaznik
         Disable Int1

         If Enkoder_stan = 2 Then
            If Parametr(param_enkoder) < Param_max_value(param_enkoder) Then Parametr(param_enkoder) = Parametr(param_enkoder) + 1
         End If

         If Enkoder_stan = 3 Then
            If Parametr(param_enkoder) > 0 Then Parametr(param_enkoder) = Parametr(param_enkoder) - 1
         End If

         Enkoder_stan = 0
         Enable Int1
      '--------------------------------------------------------

   Parametr(5) = Parametr(111)                              'przepisanie bypass AGC z parametru 111 na 5 (poniewaz uC1 nie czyta ramu tak wysoko)

         'nadawanie wszystkich parametrow do modulow
         Cs = 0

         If Zmiana_flag = 1 Then
            'limiter
            Call Tc_write(parametr(100) , Parametr(105) , 0)

            'bas pierwszego , srodek pierwszego
            Call Tc_write(parametr(60) , Parametr(65) , 1)

            'gora pierwszego, bas drugiego
            Call Tc_write(parametr(70) , Parametr(75) , 2)

            'srodek drugiego, gora drugiego
            Call Tc_write(parametr(80) , Parametr(85) , 3)
         End If



         If Zmiana_flag = 1 Then
            'wake up
            Call Wms_write(1 , 16 , 0 , 16 , 0 , 16 , 0)
            Call Wms_write(2 , 16 , 0 , 16 , 0 , 16 , 0)
            Call Wms_write(3 , 16 , 0 , 16 , 0 , 16 , 0)

            Dim Tmp_wms1 As Byte
            Dim Tmp_wms2 As Byte
            Dim Tmp_wms3 As Byte



            'attack, release
            Tmp_wms1 = Attack2wms(parametr(78))
            Tmp_wms2 = Attack2wms(parametr(68))
            Tmp_wms3 = Attack2wms(parametr(58))
            Call Wms_write(1 , 64 , Tmp_wms1 , 64 , Tmp_wms2 , 64 , Tmp_wms3)       '5A, 3A, 1A attack

            Tmp_wms1 = Release2wms(parametr(79))
            Tmp_wms2 = Release2wms(parametr(69))
            Tmp_wms3 = Release2wms(parametr(59))
            Call Wms_write(1 , 65 , Tmp_wms1 , 65 , Tmp_wms2 , 65 , Tmp_wms3)       '5R, 3R, 1R release

            Tmp_wms1 = Attack2wms(parametr(83))
            Tmp_wms2 = Attack2wms(parametr(73))
            Tmp_wms3 = Attack2wms(parametr(63))
            Call Wms_write(1 , 66 , Tmp_wms1 , 66 , Tmp_wms2 , 66 , Tmp_wms3)       '6A, 4A, 2A attack

            Tmp_wms1 = Release2wms(parametr(84))
            Tmp_wms2 = Release2wms(parametr(74))
            Tmp_wms3 = Release2wms(parametr(64))
            Call Wms_write(1 , 67 , Tmp_wms1 , 67 , Tmp_wms2 , 67 , Tmp_wms3)       '6R, 4R, 2R release


            'threshold, ratio
            Tmp_wms1 = Ratio2wms(parametr(67))
            Tmp_wms2 = Ratio2wms(parametr(62))
            Tmp_wms3 = Ratio2wms(parametr(57))
            Call Wms_write(2 , 64 , Tmp_wms1 , 64 , Tmp_wms2 , 64 , Tmp_wms3)       'ratio (lewy) (1, 2, ostatnia w kolejce plytka)

            Tmp_wms1 = Ratio2wms(parametr(82))
            Tmp_wms2 = Ratio2wms(parametr(77))
            Tmp_wms3 = Ratio2wms(parametr(72))
            Call Wms_write(2 , 65 , Tmp_wms1 , 65 , Tmp_wms2 , 65 , Tmp_wms3)       'ratio (prawy)


            Tmp_wms1 = Thres2wms(parametr(66))
            Tmp_wms2 = Thres2wms(parametr(61))
            Tmp_wms3 = Thres2wms(parametr(56))
            Call Wms_write(2 , 66 , Tmp_wms1 , 66 , Tmp_wms2 , 66 , Tmp_wms2)       'threshold (lewy) ok

            Tmp_wms1 = Thres2wms(parametr(81))
            Tmp_wms2 = Thres2wms(parametr(76))
            Tmp_wms3 = Thres2wms(parametr(71))
            Call Wms_write(2 , 67 , Tmp_wms1 , 67 , Tmp_wms2 , 67 , Tmp_wms2)       'threshold (prawy)  ok


            'plytka LIM_AR
            Call Wms_write(3 , 64 , 127 , 64 , 127 , 64 , 255)       'pusty, AR, ratio
            Call Wms_write(3 , 65 , 127 , 65 , 127 , 65 , 255)       'pusty, AR, ratio
            Call Wms_write(3 , 66 , 127 , 66 , 127 , 66 , 40)       'pusty, AR, threshold
            Call Wms_write(3 , 67 , 127 , 67 , 127 , 67 , 40)       'pusty, AR, threshold


            'rejestr przesuwny na plytce SUM
            If Parametr(113) = 0 Then                       'jesli bypass limitera wlaczony
               Tmp_wms1 = 2
            Else
               Tmp_wms1 = 2 - Parametr(106)
            End If
            Call Przesuwny_write(tmp_wms1 , Parametr(115))

        End If

         'znak konca pracy - diagnostyczny
         Call Set_cs_adr(10)
         Cs = 0
         Waitus 200
         Cs = 1
         Waitus 200
         Cs = 0
         Call Set_cs_adr(0)


         Zmiana_flag = 0
         Przerwanie_flag = 0

   End If


Loop


'przerwanie od uC AGC
Poczatek:
Przerwanie_flag = 1
Return



'generowanie wiersza nr 1 (tytul menu)
Sub Oblicz_wiersz1(menu As Byte)

If Menu < 10 Then Wierszx = "[0" + Str(menu) + "] "
If Menu >= 10 Then Wierszx = "[" + Str(menu) + "] "


 If Menu = 1 Then Wierszx = Wierszx + "AGC Left               "
 If Menu = 2 Then Wierszx = Wierszx + "AGC Right              "
 If Menu = 3 Then Wierszx = Wierszx + "AGC Left Speed Up      "
 If Menu = 4 Then Wierszx = Wierszx + "AGC Right Speed Up     "
 If Menu = 5 Then Wierszx = Wierszx + "AGC Left Speed Down    "
 If Menu = 6 Then Wierszx = Wierszx + "AGC Right Speed Down   "
 If Menu = 7 Then Wierszx = Wierszx + "AGC Left Section Level "
 If Menu = 8 Then Wierszx = Wierszx + "AGC Right Section Level"
 If Menu = 9 Then Wierszx = Wierszx + "AGC Left Section Ratio "
If Menu = 10 Then Wierszx = Wierszx + "AGC Right Section Ratio"
If Menu = 11 Then Wierszx = Wierszx + "Filter                 "
If Menu = 12 Then Wierszx = Wierszx + "Compressor Left Low    "
If Menu = 13 Then Wierszx = Wierszx + "Compressor Left Mid    "
If Menu = 14 Then Wierszx = Wierszx + "Compressor Left Hi     "
If Menu = 15 Then Wierszx = Wierszx + "Compressor Right Low   "
If Menu = 16 Then Wierszx = Wierszx + "Compressor Right Mid   "
If Menu = 17 Then Wierszx = Wierszx + "Compressor Right Hi    "
If Menu = 18 Then Wierszx = Wierszx + "Compressor Mode Left   "
If Menu = 19 Then Wierszx = Wierszx + "Compressor Mode Right  "
If Menu = 20 Then Wierszx = Wierszx + "Limiter Left           "
If Menu = 21 Then Wierszx = Wierszx + "Limiter Right          "
If Menu = 22 Then Wierszx = Wierszx + "Limiter Mode           "
If Menu = 23 Then Wierszx = Wierszx + "Functions              "
If Menu = 24 Then Wierszx = Wierszx + "Display Mode           "
If Menu = 50 Then Wierszx = Wierszx + "Praca Dyplomowa PW2024 "

'dopelnienie stringa zeby mial 40 znakow
Wierszx = Wierszx + "            "

'budowa tablicy string z tekstem
For I = 1 To 40
Tmp_str = Mid(wierszx , I , 1)
Lcd_table(i) = Asc(tmp_str)
Next I

End Sub


'wiersz nr 2
Sub Oblicz_wiersz2(menu As Byte)

'........................................>xxxxxx<>xxxxxx<>xxxxxx<>xxxxxx<>xxxxxx<.....
If Menu = 1 Or Menu = 2 Then Wierszx = " Algor   Thresh  EndLvl  MaxAGC         "
If Menu = 3 Or Menu = 4 Then Wierszx = " SectU1  SectU2  SectU3  SectU4  SectU5 "
If Menu = 5 Or Menu = 6 Then Wierszx = " SectD1  SectD2  SectD3  SectD4  SectD5 "
If Menu = 7 Or Menu = 8 Then Wierszx = " Level1  Level2  Level3  Level4  Level5 "
If Menu = 9 Or Menu = 10 Then Wierszx = " Ratio1  Ratio2  Ratio3  Ratio4  Ratio5 "
If Menu = 11 Then Wierszx = " L Low    L Hi   R Low    R Hi          "
If Menu = 12 Or Menu = 13 Or Menu = 14 Or Menu = 15 Or Menu = 16 Or Menu = 17 Or Menu = 20 Or Menu = 21 Then Wierszx = " Thresh  Ratio   Attack  Releas  Output "
If Menu = 18 Or Menu = 19 Then Wierszx = "  Mode    Mode    Mode                  "
If Menu = 22 Then Wierszx = "  Mode                                  "
If Menu = 23 Then Wierszx = "   AGC     CMP     LIM             All  "
If Menu = 24 Then Wierszx = " Displ1  Displ2  Displ3  Displ4  Displ5 "       'to menu jest niedostepne
If Menu = 50 Then Wierszx = "Kamil Syzdol      firmw. v21  28.01.2024"

'budowa tablicy string z tekstem
For I = 1 To 40
Tmp_str = Mid(wierszx , I , 1)
Lcd_table(i) = Asc(tmp_str)
Next I

End Sub


'wiersz nr 3
Sub Oblicz_wiersz3(menu As Byte , Wskaznik As Byte)
If Menu = 1 Then
Menu_field1 = Lookupstr(parametr(1) , Lista(6) ) : Menu_field2 = Lookupstr(parametr(2) , Lista(3) ) : Menu_field3 = Lookupstr(parametr(3) , Lista(4) ) : Menu_field4 = Lookupstr(parametr(4) , Lista(5) ) : Menu_field5 = Lookupstr(parametr(5) , Lista(1) )
End If

If Menu = 2 Then
Menu_field1 = Lookupstr(parametr(6) , Lista(6) ) : Menu_field2 = Lookupstr(parametr(7) , Lista(3) ) : Menu_field3 = Lookupstr(parametr(8) , Lista(4) ) : Menu_field4 = Lookupstr(parametr(9) , Lista(5) ) : Menu_field5 = Lookupstr(parametr(10) , Lista(1) )
End If

If Menu = 3 Then
Menu_field1 = Lookupstr(parametr(11) , Lista(7) ) : Menu_field2 = Lookupstr(parametr(12) , Lista(7) ) : Menu_field3 = Lookupstr(parametr(13) , Lista(7) ) : Menu_field4 = Lookupstr(parametr(14) , Lista(7) ) : Menu_field5 = Lookupstr(parametr(15) , Lista(7) )
End If

If Menu = 4 Then
Menu_field1 = Lookupstr(parametr(16) , Lista(7) ) : Menu_field2 = Lookupstr(parametr(17) , Lista(7) ) : Menu_field3 = Lookupstr(parametr(18) , Lista(7) ) : Menu_field4 = Lookupstr(parametr(19) , Lista(7) ) : Menu_field5 = Lookupstr(parametr(20) , Lista(7) )
End If

If Menu = 5 Then
Menu_field1 = Lookupstr(parametr(21) , Lista(7) ) : Menu_field2 = Lookupstr(parametr(22) , Lista(7) ) : Menu_field3 = Lookupstr(parametr(23) , Lista(7) ) : Menu_field4 = Lookupstr(parametr(24) , Lista(7) ) : Menu_field5 = Lookupstr(parametr(25) , Lista(7) )
End If

If Menu = 6 Then
Menu_field1 = Lookupstr(parametr(26) , Lista(7) ) : Menu_field2 = Lookupstr(parametr(27) , Lista(7) ) : Menu_field3 = Lookupstr(parametr(28) , Lista(7) ) : Menu_field4 = Lookupstr(parametr(29) , Lista(7) ) : Menu_field5 = Lookupstr(parametr(30) , Lista(7) )
End If

If Menu = 7 Then
Menu_field1 = Lookupstr(parametr(31) , Lista(8) ) : Menu_field2 = Lookupstr(parametr(32) , Lista(8) ) : Menu_field3 = Lookupstr(parametr(33) , Lista(8) ) : Menu_field4 = Lookupstr(parametr(34) , Lista(8) ) : Menu_field5 = Lookupstr(parametr(35) , Lista(8) )
End If

If Menu = 8 Then
Menu_field1 = Lookupstr(parametr(36) , Lista(8) ) : Menu_field2 = Lookupstr(parametr(37) , Lista(8) ) : Menu_field3 = Lookupstr(parametr(38) , Lista(8) ) : Menu_field4 = Lookupstr(parametr(39) , Lista(8) ) : Menu_field5 = Lookupstr(parametr(40) , Lista(8) )
End If

If Menu = 9 Then
Menu_field1 = Lookupstr(parametr(41) , Lista(9) ) : Menu_field2 = Lookupstr(parametr(42) , Lista(9) ) : Menu_field3 = Lookupstr(parametr(43) , Lista(9) ) : Menu_field4 = Lookupstr(parametr(44) , Lista(9) ) : Menu_field5 = Lookupstr(parametr(45) , Lista(9) )
End If

If Menu = 10 Then
Menu_field1 = Lookupstr(parametr(46) , Lista(9) ) : Menu_field2 = Lookupstr(parametr(47) , Lista(9) ) : Menu_field3 = Lookupstr(parametr(48) , Lista(9) ) : Menu_field4 = Lookupstr(parametr(49) , Lista(9) ) : Menu_field5 = Lookupstr(parametr(50) , Lista(9) )
End If

If Menu = 11 Then
Menu_field1 = Lookupstr(parametr(51) , Lista(10) ) : Menu_field2 = Lookupstr(parametr(52) , Lista(11) ) : Menu_field3 = Lookupstr(parametr(53) , Lista(10) ) : Menu_field4 = Lookupstr(parametr(54) , Lista(11) ) : Menu_field5 = Lookupstr(parametr(55) , Lista(1) )
End If

If Menu = 12 Then
Menu_field1 = Lookupstr(parametr(56) , Lista(12) ) : Menu_field2 = Lookupstr(parametr(57) , Lista(13) ) : Menu_field3 = Lookupstr(parametr(58) , Lista(14) ) : Menu_field4 = Lookupstr(parametr(59) , Lista(15) ) : Menu_field5 = Lookupstr(parametr(60) , Lista(16) )
End If

If Menu = 13 Then
Menu_field1 = Lookupstr(parametr(61) , Lista(12) ) : Menu_field2 = Lookupstr(parametr(62) , Lista(13) ) : Menu_field3 = Lookupstr(parametr(63) , Lista(14) ) : Menu_field4 = Lookupstr(parametr(64) , Lista(15) ) : Menu_field5 = Lookupstr(parametr(65) , Lista(16) )
End If

If Menu = 14 Then
Menu_field1 = Lookupstr(parametr(66) , Lista(12) ) : Menu_field2 = Lookupstr(parametr(67) , Lista(13) ) : Menu_field3 = Lookupstr(parametr(68) , Lista(14) ) : Menu_field4 = Lookupstr(parametr(69) , Lista(15) ) : Menu_field5 = Lookupstr(parametr(70) , Lista(16) )
End If

If Menu = 15 Then
Menu_field1 = Lookupstr(parametr(71) , Lista(12) ) : Menu_field2 = Lookupstr(parametr(72) , Lista(13) ) : Menu_field3 = Lookupstr(parametr(73) , Lista(14) ) : Menu_field4 = Lookupstr(parametr(74) , Lista(15) ) : Menu_field5 = Lookupstr(parametr(75) , Lista(16) )
End If

If Menu = 16 Then
Menu_field1 = Lookupstr(parametr(76) , Lista(12) ) : Menu_field2 = Lookupstr(parametr(77) , Lista(13) ) : Menu_field3 = Lookupstr(parametr(78) , Lista(14) ) : Menu_field4 = Lookupstr(parametr(79) , Lista(15) ) : Menu_field5 = Lookupstr(parametr(80) , Lista(16) )
End If

If Menu = 17 Then
Menu_field1 = Lookupstr(parametr(81) , Lista(12) ) : Menu_field2 = Lookupstr(parametr(82) , Lista(13) ) : Menu_field3 = Lookupstr(parametr(83) , Lista(14) ) : Menu_field4 = Lookupstr(parametr(84) , Lista(15) ) : Menu_field5 = Lookupstr(parametr(85) , Lista(16) )
End If

If Menu = 18 Then
Menu_field1 = Lookupstr(parametr(86) , Lista(17) ) : Menu_field2 = Lookupstr(parametr(87) , Lista(17) ) : Menu_field3 = Lookupstr(parametr(88) , Lista(17) ) : Menu_field4 = Lookupstr(parametr(89) , Lista(1) ) : Menu_field5 = Lookupstr(parametr(90) , Lista(1) )
End If

If Menu = 19 Then
Menu_field1 = Lookupstr(parametr(91) , Lista(17) ) : Menu_field2 = Lookupstr(parametr(92) , Lista(17) ) : Menu_field3 = Lookupstr(parametr(93) , Lista(17) ) : Menu_field4 = Lookupstr(parametr(94) , Lista(1) ) : Menu_field5 = Lookupstr(parametr(95) , Lista(1) )
End If

If Menu = 20 Then
Menu_field1 = Lookupstr(parametr(96) , Lista(12) ) : Menu_field2 = Lookupstr(parametr(97) , Lista(13) ) : Menu_field3 = Lookupstr(parametr(98) , Lista(18) ) : Menu_field4 = Lookupstr(parametr(99) , Lista(19) ) : Menu_field5 = Lookupstr(parametr(100) , Lista(16) )
End If

If Menu = 21 Then
Menu_field1 = Lookupstr(parametr(101) , Lista(12) ) : Menu_field2 = Lookupstr(parametr(102) , Lista(13) ) : Menu_field3 = Lookupstr(parametr(103) , Lista(18) ) : Menu_field4 = Lookupstr(parametr(104) , Lista(19) ) : Menu_field5 = Lookupstr(parametr(105) , Lista(16) )
End If

If Menu = 22 Then
Menu_field1 = Lookupstr(parametr(106) , Lista(17) ) : Menu_field2 = Lookupstr(parametr(107) , Lista(1) ) : Menu_field3 = Lookupstr(parametr(108) , Lista(1) ) : Menu_field4 = Lookupstr(parametr(109) , Lista(1) ) : Menu_field5 = Lookupstr(parametr(110) , Lista(1) )
End If

If Menu = 23 Then
Menu_field1 = Lookupstr(parametr(111) , Lista(2) ) : Menu_field2 = Lookupstr(parametr(112) , Lista(2) ) : Menu_field3 = Lookupstr(parametr(113) , Lista(2) ) : Menu_field4 = Lookupstr(parametr(114) , Lista(1) ) : Menu_field5 = Lookupstr(parametr(115) , Lista(2) )
End If

If Menu = 24 Then
Menu_field1 = Lookupstr(parametr(116) , Lista(1) ) : Menu_field2 = Lookupstr(parametr(117) , Lista(1) ) : Menu_field3 = Lookupstr(parametr(118) , Lista(1) ) : Menu_field4 = Lookupstr(parametr(119) , Lista(1) ) : Menu_field5 = Lookupstr(parametr(120) , Lista(1) )
End If

If Menu = 25 Then
Menu_field1 = Lookupstr(parametr(121) , Lista(1) ) : Menu_field2 = Lookupstr(parametr(122) , Lista(1) ) : Menu_field3 = Lookupstr(parametr(123) , Lista(1) ) : Menu_field4 = Lookupstr(parametr(124) , Lista(1) ) : Menu_field5 = Lookupstr(parametr(125) , Lista(1) )
End If

If Menu = 50 Then
Menu_field1 = Lookupstr(parametr(121) , Lista(1) ) : Menu_field2 = Lookupstr(parametr(122) , Lista(1) ) : Menu_field3 = Lookupstr(parametr(123) , Lista(1) ) : Menu_field4 = Lookupstr(parametr(124) , Lista(1) ) : Menu_field5 = Lookupstr(parametr(125) , Lista(1) )
End If


'jesli mozna edytowac parametr
If Param_max_value(param_enkoder) <> 0 Then
'jesli wskaznik jest spoza przedzialu 1...5 to wtedy go nie wyswietlaj
   If Wskaznik = 1 Then Wierszx = ">" + Menu_field1 + "< " + Menu_field2 + "  " + Menu_field3 + "  " + Menu_field4 + "  " + Menu_field5 + " "
   If Wskaznik = 2 Then Wierszx = " " + Menu_field1 + " >" + Menu_field2 + "< " + Menu_field3 + "  " + Menu_field4 + "  " + Menu_field5 + " "
   If Wskaznik = 3 Then Wierszx = " " + Menu_field1 + "  " + Menu_field2 + " >" + Menu_field3 + "< " + Menu_field4 + "  " + Menu_field5 + " "
   If Wskaznik = 4 Then Wierszx = " " + Menu_field1 + "  " + Menu_field2 + "  " + Menu_field3 + " >" + Menu_field4 + "< " + Menu_field5 + " "
   If Wskaznik = 5 Then Wierszx = " " + Menu_field1 + "  " + Menu_field2 + "  " + Menu_field3 + "  " + Menu_field4 + " >" + Menu_field5 + "<"
   If Wskaznik < 1 Or Wskaznik > 5 Then Wierszx = " " + Menu_field1 + "  " + Menu_field2 + "  " + Menu_field3 + "  " + Menu_field4 + "  " + Menu_field5 + " "
Else
   'jesli nie mozna edytowac
   Wierszx = " " + Menu_field1 + "  " + Menu_field2 + "  " + Menu_field3 + "  " + Menu_field4 + "  " + Menu_field5 + " "
End If


For I = 1 To 40
Tmp_byte = I + 40
Tmp_str = Mid(wierszx , I , 1)
Lcd_table(tmp_byte) = Asc(tmp_str)
Next I


End Sub



'ktory blok parametrow lokalnych; pierwszy adres z zewnetrznej pamieci, ilosc danych
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


   'nadawanie adresu poczatku strony (16-bit starsza czesc)  (00000001 od 256)
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




'ktory blok parametrow lokalnych; pierwszy adres z zewnetrznej pamieci, ilosc danych
Sub Ram_page_read(ram_page_read_param As Byte , Ram_page_read_adr As Byte , Ram_page_read_ile , Ram_page_read_bank As Byte)

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


   'nadawanie adresu poczatku strony (16-bit, starsza czesc) (00000001, od 256) (00000000, od 0)
         Ram_page_read_wyslac = Ram_page_read_bank
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

            Parametr_odczyt_ram(ram_page_write_param) = Ram_page_read_odebrane

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


'zapisywanie do RAM status register
Sub Ram_write_sreg()

   'konfiguracja pinow
   Config Ram_cs = Output
   Config Ram_clk = Output
   Config Ram_sdi = Output
   Config Ram_sdo = Input

    Ram_cs = 0

    'zabezpieczanie rejestrow do pamieci sram
    !STS {Ram_r21}, R21
    !STS {Ram_r23}, R23

   'nadawanie komendy write sreg
         !LDI R21, 1                                        'wrzucam do R21 dane do wyslania
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_write_sreg1:
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
            !JMP Petla_write_sreg1                          'powrot na poczatek petli
            !nop
            !CBI PORTD, 7                                   'clk=0
            !nop

   'nadawanie tresci write sreg
         !LDI R21, 0                                        'wrzucam do R21 dane do wyslania
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_write_sreg2:
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
            !JMP Petla_write_sreg2                          'powrot na poczatek petli
            !nop
            !CBI PORTD, 7                                   'clk=0
            !nop

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


'odczytywanie z RAM status register
Sub Ram_read_sreg()

   'konfiguracja pinow
   Config Ram_cs = Output
   Config Ram_clk = Output
   Config Ram_sdi = Output
   Config Ram_sdo = Input
    Ram_cs = 0

    'zabezpieczanie rejestrow do pamieci sram
    !STS {Ram_r21}, R21
    !STS {Ram_r23}, R23

   'nadawanie komendy rea sreg
         !LDI R21, 5                                        'wrzucam do R21 dane do wyslania
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_read_sreg1:
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
            !JMP Petla_read_sreg1                           'powrot na poczatek petli
            !nop
            !CBI PORTD, 7                                   'clk=0
            !nop

   'nadawanie tresci read sreg
         !LDI R21, 0                                        'wrzucam do R21 dane do wyslania
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_read_sreg2:
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
            !JMP Petla_read_sreg2                           'powrot na poczatek petli
            !nop
            !CBI PORTD, 7                                   'clk=0
            !nop

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


'wysylanie do RAM zawartosci tekstu na LCD
Sub Ram_lcd_write()


   'konfiguracja pinow
   Config Ram_cs = Output
   Config Ram_clk = Output
   Config Ram_sdi = Output
   Config Ram_sdo = Input


   Dim Ram_lcd_write_loop As Byte
   Dim Ram_lcd_write_param_first_adr As Byte
   Dim Ram_lcd_write_param_last_adr As Byte

   Dim Ram_lcd_write_wyslac As Byte

   Dim Ram_lcd_write_param_adr As Byte


    Ram_cs = 0

    'zabezpieczanie rejestrow do pamieci sram
    !STS {Ram_r21}, R21
    !STS {Ram_r23}, R23

   'nadawanie komendy write
         !LDI R21, 2                                        'wrzucam do R21 komende write
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_lcd_write1:
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
            !JMP Petla_lcd_write1                           'powrot na poczatek petli
            !nop
            !CBI PORTD, 7                                   'clk=0
            !nop


   'nadawanie adresu poczatku strony (16-bit starsza czesc)  (00000001 od 256)
         Ram_lcd_write_wyslac = 1
         !LDS R21, {Ram_lcd_write_wyslac}                   'wrzucam do R21 pierwszy adres
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_lcd_write2:
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
            !JMP Petla_lcd_write2                           'powrot na poczatek petli
            !nop
            !CBI PORTD, 7                                   'clk=0
            !nop



   'nadawanie adresu poczatku strony (16-bit mlodsza czesc)
         Ram_lcd_write_wyslac = 0
         !LDS R21, {Ram_lcd_write_wyslac}                   'wrzucam do R21 pierwszy adres
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_lcd_write3:
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
            !JMP Petla_lcd_write3                           'powrot na poczatek petli
            !nop
            !CBI PORTD, 7                                   'clk=0
            !nop


   'nadawanie danych
   For Ram_lcd_write_loop = 1 To 80


      Ram_lcd_write_wyslac = Lcd_table(ram_lcd_write_loop)

         !LDS R21, {ram_lcd_write_wyslac}                   'wrzucam do R21 dane do wyslania
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_lcd_write4:
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
            !JMP Petla_lcd_write4                           'powrot na poczatek petli
            !nop
            !CBI PORTD, 7                                   'clk=0

            !nop

      Next Ram_lcd_write_loop


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



'obsluga demultipleksera 74154 do sygnalu CS lub STROBE
Sub Set_cs_adr(byval Cs_adr As Byte)
   Cs_adr = Cs_adr And 15                                   'maska 0b00001111
   Shift Cs_adr , Left , 2

   'najpierw zeruje dane koncowki, a potem wstawiam im docelowe wartosci
   Portb = Portb And 195                                    '0b11000011
   Portb = Portb Or Cs_adr
End Sub



'klawiatura
Sub Kbd_read()

Config Tm_clk = Output
Config Tm_sdio = Output

   'ustawianie wlasciwego CS
   Cs = 1
   Call Set_cs_adr(12)
   Tm_sdio = 0
   Cs = 0

    'zabezpieczanie rejestrow do pamieci sram
    !STS {Ram_r21}, R21
    !STS {Ram_r23}, R23

   'nadawanie komendy keyboard read
         !LDI R21, 66                                       'wrzucam do R21 komende read keyboard
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_kbd_read1:
            !CBI PORTC, 4                                   'clk=0

            !SBRS R21,0                                     'sprawdz czy 1 w bicie 0 i jesli tak, to pomin nastepne
            !Cbi PORTC, 3                                   'ustaw 0 na sdo
            !SBRC R21,0                                     'sprawdz czy 0 w bicie  0 i jesli tak, to pomin nastepne
            !SBI PORTC, 3                                   'ustaw 1 na sdo

            !NOP
            !SBI PORTC, 4                                   'clk=1

            !LSR R21                                        'przesun rejestr w prawo

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_kbd_read1                            'powrot na poczatek petli
            !nop
            !CBI PORTC, 4                                   'clk=0
            !nop


            Config Tm_sdio = Input
            Waitus 5



   'odbieranie danych
   For I = 1 To 4

         !clr R21                                           'zeruje R21 - przygotowuje do odbioru
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_kbd_read2:
            !SBI PORTC, 4                                   'clk=1

            !LSR R21                                        'przesun rejestr w lewo
            !ANDI R21, 127                                  'ustaw pierwszy bit z lewej na 0 (R23 AND 01111111)


            !SBIC PINC, PINC3                               ' sprawdzam czy na PD6 jest 1
            !ORI R21, 128                                   'tu nalezy ustawic pierwszy bit z lewej na 1 (R23 OR 10000000)
'            !NOP

            !CBI PORTC, 4                                   'clk=0

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_kbd_read2                            'powrot na poczatek petli
'            !nop
            !CBI PORTC, 4                                   'clk=0

            !nop

            'wrzucanie do parametru zawartosci rejestru R21
            !STS {kbd_odebrane}, R21

            Kbd_read_table(i) = Kbd_odebrane

          '  Ram_page_write_param = Ram_page_write_param + 1

      Next I

   Cs = 1
   Config Tm_clk = Input
   Config Tm_sdio = Input

   Call Set_cs_adr(0)





End Sub

'przerwanie z INT1 od enkodera
Przerwanie_enkoder:
   If Pind.4 = 0 Then
      Enkoder_stan = 2
   Else
      Enkoder_stan = 3
   End If
   'stawiam flage do wysylki danych do WMS, TC i 4094, poniewaz wraz z obrotem enkodera najczesciej zmieniaja sie tez dane
   Zmiana_flag = 1
Return

'nadawanie do TC9459F
Sub Tc_write(byval Tc_left As Byte , Byval Tc_right As Byte , Bycal Tc_adr As Byte)

   Tc_left = Tc_left + 111
   Tc_right = Tc_right + 111
   If Tc_left > 127 Then Tc_left = 127
   If Tc_right > 127 Then Tc_right = 127

   If Tc_left < 37 Then Tc_left = 37
   If Tc_right < 37 Then Tc_right = 37

   Tc_left = 127 - Tc_left
   Tc_right = 127 - Tc_right



   Shift Tc_adr , Left , 4
   Tc_adr = Tc_adr And 48
   Tc_adr = Tc_adr Or 128
   Dim Tc_write_wyslac As Byte


   Cs = 1
   Call Set_cs_adr(7)

    'zabezpieczanie rejestrow do pamieci sram
    !STS {Ram_r21}, R21
    !STS {Ram_r23}, R23


   'nadawanie danych lewego kanalu
         Tc_write_wyslac = Tc_left
         !LDS R21, {tc_write_wyslac}                        'wrzucam do R21 pierwszy adres
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_tc_write1:
            !CBI PORTC, 0                                   'clk=0

            !SBRS R21,0                                     'sprawdz czy 1 w bicie 0 i jesli tak, to pomin nastepne
            !Cbi PORTC, 1                                   'ustaw 0 na sdo
            !SBRC R21,0                                     'sprawdz czy 0 w bicie  0 i jesli tak, to pomin nastepne
            !SBI PORTC, 1                                   'ustaw 1 na sdo

            !NOP
            !SBI PORTC, 0                                   'clk=1

            !LSR R21                                        'przesun rejestr w lewo

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_tc_write1                            'powrot na poczatek petli
            !nop
            !CBI PORTC, 0                                   'clk=0
            !nop

   'nadawanie danych prawego kanalu
         Tc_write_wyslac = Tc_right
         !LDS R21, {tc_write_wyslac}                        'wrzucam do R21 pierwszy adres
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_tc_write2:
            !CBI PORTC, 0                                   'clk=0

            !SBRS R21,0                                     'sprawdz czy 1 w bicie 0 i jesli tak, to pomin nastepne
            !Cbi PORTC, 1                                   'ustaw 0 na sdo
            !SBRC R21,0                                     'sprawdz czy 0 w bicie 0 i jesli tak, to pomin nastepne
            !SBI PORTC, 1                                   'ustaw 1 na sdo

            !NOP
            !SBI PORTC, 0                                   'clk=1

            !LSR R21                                        'przesun rejestr w lewo

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_tc_write2                            'powrot na poczatek petli
            !nop
            !CBI PORTC, 0                                   'clk=0
            !nop

   'nadawanie danych loudness i adresu
         Tc_write_wyslac = Tc_adr
         !LDS R21, {tc_write_wyslac}                        'wrzucam do R21 pierwszy adres
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_tc_write3:
            !CBI PORTC, 0                                   'clk=0

            !SBRS R21,0                                     'sprawdz czy 1 w bicie 0 i jesli tak, to pomin nastepne
            !Cbi PORTC, 1                                   'ustaw 0 na sdo
            !SBRC R21,0                                     'sprawdz czy 0 w bicie 0 i jesli tak, to pomin nastepne
            !SBI PORTC, 1                                   'ustaw 1 na sdo

            !NOP
            !SBI PORTC, 0                                   'clk=1

            !LSR R21                                        'przesun rejestr w lewo

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_tc_write3                            'powrot na poczatek petli
            !nop
            !CBI PORTC, 0                                   'clk=0
            !nop


   'strobe
            !NOP
            !NOP

            Cs = 0
            !NOP
            !NOP
   'przywracanie rejestrow z pamieci sram
    !LDS R21, {Ram_r21}
    !LDS R23, {Ram_r23}


            Cs = 1



            Call Set_cs_adr(0)

End Sub

'nadawanie do rejestru przesuwnego 4094
Sub Przesuwny_write(byval Lim_mux As Byte , Byval Bypass_pk As Byte)

   Dim Przesuwny_write_wyslac As Byte
   If Lim_mux > 3 Then Lim_mux = 3
   If Bypass_pk > 1 Then Bypass_pk = 1

   Lim_mux = Lim_mux And &B00000011

   Shift Lim_mux , Left , 6
   Lim_mux = Lim_mux And &B11000000
   Przesuwny_write_wyslac = Lim_mux + Bypass_pk
   Cs = 1
   Call Set_cs_adr(6)

    'zabezpieczanie rejestrow do pamieci sram
    !STS {Ram_r21}, R21
    !STS {Ram_r23}, R23


   'nadawanie danych

         !LDS R21, {przesuwny_write_wyslac}                        'wrzucam do R21 pierwszy adres
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_przesuwny_write1:
            !CBI PORTC, 0                                   'clk=0

            !SBRS R21,0                                     'sprawdz czy 1 w bicie 0 i jesli tak, to pomin nastepne
            !Cbi PORTC, 1                                   'ustaw 0 na sdo
            !SBRC R21,0                                     'sprawdz czy 0 w bicie  0 i jesli tak, to pomin nastepne
            !SBI PORTC, 1                                   'ustaw 1 na sdo

            !NOP
            !SBI PORTC, 0                                   'clk=1

            !LSR R21                                        'przesun rejestr w lewo

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_przesuwny_write1                            'powrot na poczatek petli
            !nop
            !CBI PORTC, 0                                   'clk=0
            !nop


   'strobe
            !NOP
            !NOP

            Cs = 0
            !NOP
            !NOP
   'przywracanie rejestrow z pamieci sram
    !LDS R21, {Ram_r21}
    !LDS R23, {Ram_r23}


            Cs = 1



            Call Set_cs_adr(0)


End Sub

'nadawanie do WMS7204
Sub Wms_write(byval Wms_adr As Byte , Byval Wms_cmd1 As Byte , Byval Wms_dat1 As Byte , Byval Wms_cmd2 As Byte , Byval Wms_dat2 As Byte , Byval Wms_cmd3 As Byte , Byval Wms_dat3 As Byte )

Dim Wms_write_wyslac As Byte
   Cs = 1
   If Wms_adr = 1 Then Call Set_cs_adr(1)                   'cmp ar(1)
   If Wms_adr = 2 Then Call Set_cs_adr(2)                   'cmp calc (2)
   If Wms_adr = 3 Then Call Set_cs_adr(3)                   'lim ar_calc (3)
   If Wms_adr > 3 Then Call Set_cs_adr(0)                   'pusty
   If Wms_adr = 0 Then Call Set_cs_adr(0)
   Cs = 0

    'zabezpieczanie rejestrow do pamieci sram
    !STS {Ram_r21}, R21
    !STS {Ram_r23}, R23



   'nadawanie cmd1
         Wms_write_wyslac = Wms_cmd1
         !LDS R21, {wms_write_wyslac}                       'wrzucam do R21 pierwszy adres
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_wms_write1:
            !CBI PORTC, 0                                   'clk=0

            !SBRS R21,7                                     'sprawdz czy 1 w bicie 7 i jesli tak, to pomin nastepne
            !Cbi PORTC, 1                                   'ustaw 0 na sdo
            !SBRC R21,7                                     'sprawdz czy 0 w bicie 7 i jesli tak, to pomin nastepne
            !SBI PORTC, 1                                   'ustaw 1 na sdo

            !NOP
            !SBI PORTC, 0                                   'clk=1

            !LSL R21                                        'przesun rejestr w lewo

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_wms_write1                           'powrot na poczatek petli
            !nop
            !CBI PORTC, 0                                   'clk=0
            !nop


   'nadawanie datx
         Wms_write_wyslac = 0
         !LDS R21, {wms_write_wyslac}                       'wrzucam do R21 pierwszy adres
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_wms_write2:
            !CBI PORTC, 0                                   'clk=0

            !SBRS R21,7                                     'sprawdz czy 1 w bicie 7 i jesli tak, to pomin nastepne
            !Cbi PORTC, 1                                   'ustaw 0 na sdo
            !SBRC R21,7                                     'sprawdz czy 0 w bicie 7 i jesli tak, to pomin nastepne
            !SBI PORTC, 1                                   'ustaw 1 na sdo

            !NOP
            !SBI PORTC, 0                                   'clk=1

            !LSL R21                                        'przesun rejestr w lewo

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_wms_write2                           'powrot na poczatek petli
            !nop
            !CBI PORTC, 0                                   'clk=0
            !nop

   'nadawanie dat1
         Wms_write_wyslac = Wms_dat1
         !LDS R21, {wms_write_wyslac}                       'wrzucam do R21 pierwszy adres
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_wms_write3:
            !CBI PORTC, 0                                   'clk=0

            !SBRS R21,7                                     'sprawdz czy 1 w bicie 7 i jesli tak, to pomin nastepne
            !Cbi PORTC, 1                                   'ustaw 0 na sdo
            !SBRC R21,7                                     'sprawdz czy 0 w bicie 7 i jesli tak, to pomin nastepne
            !SBI PORTC, 1                                   'ustaw 1 na sdo

            !NOP
            !SBI PORTC, 0                                   'clk=1

            !LSL R21                                        'przesun rejestr w lewo

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_wms_write3                           'powrot na poczatek petli
            !nop
            !CBI PORTC, 0                                   'clk=0
            !nop



   'nadawanie cmd2
         Wms_write_wyslac = Wms_cmd2
         !LDS R21, {wms_write_wyslac}                       'wrzucam do R21 pierwszy adres
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_wms_write4:
            !CBI PORTC, 0                                   'clk=0

            !SBRS R21,7                                     'sprawdz czy 1 w bicie 7 i jesli tak, to pomin nastepne
            !Cbi PORTC, 1                                   'ustaw 0 na sdo
            !SBRC R21,7                                     'sprawdz czy 0 w bicie 7 i jesli tak, to pomin nastepne
            !SBI PORTC, 1                                   'ustaw 1 na sdo

            !NOP
            !SBI PORTC, 0                                   'clk=1

            !LSL R21                                        'przesun rejestr w lewo

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_wms_write4                           'powrot na poczatek petli
            !nop
            !CBI PORTC, 0                                   'clk=0
            !nop


   'nadawanie datx
         Wms_write_wyslac = 0
         !LDS R21, {wms_write_wyslac}                       'wrzucam do R21 pierwszy adres
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_wms_write5:
            !CBI PORTC, 0                                   'clk=0

            !SBRS R21,7                                     'sprawdz czy 1 w bicie 7 i jesli tak, to pomin nastepne
            !Cbi PORTC, 1                                   'ustaw 0 na sdo
            !SBRC R21,7                                     'sprawdz czy 0 w bicie 7 i jesli tak, to pomin nastepne
            !SBI PORTC, 1                                   'ustaw 1 na sdo

            !NOP
            !SBI PORTC, 0                                   'clk=1

            !LSL R21                                        'przesun rejestr w lewo

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_wms_write5                           'powrot na poczatek petli
            !nop
            !CBI PORTC, 0                                   'clk=0
            !nop

   'nadawanie dat2
         Wms_write_wyslac = Wms_dat2
         !LDS R21, {wms_write_wyslac}                       'wrzucam do R21 pierwszy adres
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_wms_write6:
            !CBI PORTC, 0                                   'clk=0

            !SBRS R21,7                                     'sprawdz czy 1 w bicie 7 i jesli tak, to pomin nastepne
            !Cbi PORTC, 1                                   'ustaw 0 na sdo
            !SBRC R21,7                                     'sprawdz czy 0 w bicie 7 i jesli tak, to pomin nastepne
            !SBI PORTC, 1                                   'ustaw 1 na sdo

            !NOP
            !SBI PORTC, 0                                   'clk=1

            !LSL R21                                        'przesun rejestr w lewo

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_wms_write6                           'powrot na poczatek petli
            !nop
            !CBI PORTC, 0                                   'clk=0
            !nop




   'nadawanie cmd3
         Wms_write_wyslac = Wms_cmd3
         !LDS R21, {wms_write_wyslac}                       'wrzucam do R21 pierwszy adres
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_wms_write7:
            !CBI PORTC, 0                                   'clk=0

            !SBRS R21,7                                     'sprawdz czy 1 w bicie 7 i jesli tak, to pomin nastepne
            !Cbi PORTC, 1                                   'ustaw 0 na sdo
            !SBRC R21,7                                     'sprawdz czy 0 w bicie 7 i jesli tak, to pomin nastepne
            !SBI PORTC, 1                                   'ustaw 1 na sdo

            !NOP
            !SBI PORTC, 0                                   'clk=1

            !LSL R21                                        'przesun rejestr w lewo

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_wms_write7                           'powrot na poczatek petli
            !nop
            !CBI PORTC, 0                                   'clk=0
            !nop


   'nadawanie datx
         Wms_write_wyslac = 0
         !LDS R21, {wms_write_wyslac}                       'wrzucam do R21 pierwszy adres
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_wms_write8:
            !CBI PORTC, 0                                   'clk=0

            !SBRS R21,7                                     'sprawdz czy 1 w bicie 7 i jesli tak, to pomin nastepne
            !Cbi PORTC, 1                                   'ustaw 0 na sdo
            !SBRC R21,7                                     'sprawdz czy 0 w bicie 7 i jesli tak, to pomin nastepne
            !SBI PORTC, 1                                   'ustaw 1 na sdo

            !NOP
            !SBI PORTC, 0                                   'clk=1

            !LSL R21                                        'przesun rejestr w lewo

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_wms_write8                           'powrot na poczatek petli
            !nop
            !CBI PORTC, 0                                   'clk=0
            !nop

   'nadawanie dat3
         Wms_write_wyslac = Wms_dat3
         !LDS R21, {wms_write_wyslac}                       'wrzucam do R21 pierwszy adres
         !CLR R23                                           'zerowanie R23 - licznik petli

         !Petla_wms_write9:
            !CBI PORTC, 0                                   'clk=0

            !SBRS R21,7                                     'sprawdz czy 1 w bicie 7 i jesli tak, to pomin nastepne
            !Cbi PORTC, 1                                   'ustaw 0 na sdo
            !SBRC R21,7                                     'sprawdz czy 0 w bicie 7 i jesli tak, to pomin nastepne
            !SBI PORTC, 1                                   'ustaw 1 na sdo

            !NOP
            !SBI PORTC, 0                                   'clk=1

            !LSL R21                                        'przesun rejestr w lewo

            !INC R23                                        'zwieksz rejestr licznika
            !SBRS R23, 3                                    'jesli 3 bit=1 to przeskocz (00001000)
            !JMP Petla_wms_write9                           'powrot na poczatek petli
            !nop
            !CBI PORTC, 0                                   'clk=0
            !nop


    !nop
    !nop
    !nop

   'przywracanie rejestrow z pamieci sram
    !LDS R21, {Ram_r21}
    !LDS R23, {Ram_r23}


    Cs = 1
    Call Set_cs_adr(0)

End Sub

'konwersja parametru Threshold na wartosc nadawana do WMS
Function Thres2wms(threswms)
   Thres2wms = Threswms
   If Threswms = 0 Then Thres2wms = 10 : If Threswms = 1 Then Thres2wms = 11 : If Threswms = 2 Then Thres2wms = 12 : If Threswms = 3 Then Thres2wms = 13
   If Threswms = 4 Then Thres2wms = 15 : If Threswms = 5 Then Thres2wms = 16 : If Threswms = 6 Then Thres2wms = 18 : If Threswms = 7 Then Thres2wms = 20
   If Threswms = 8 Then Thres2wms = 22 : If Threswms = 9 Then Thres2wms = 24 : If Threswms = 10 Then Thres2wms = 26 : If Threswms = 11 Then Thres2wms = 29
   If Threswms = 12 Then Thres2wms = 33 : If Threswms = 13 Then Thres2wms = 36 : If Threswms = 14 Then Thres2wms = 40 : If Threswms = 15 Then Thres2wms = 46
   If Threswms = 16 Then Thres2wms = 52 : If Threswms = 17 Then Thres2wms = 59 : If Threswms = 18 Then Thres2wms = 67 : If Threswms = 19 Then Thres2wms = 77
   If Threswms = 20 Then Thres2wms = 85
   If Threswms > 20 Then Thres2wms = 127
End Function

'konwersja parametru Ratio na wartosc nadawana do WMS
Function Ratio2wms(ratiowms)
   Ratio2wms = Ratiowms
   If Ratiowms = 0 Then Ratio2wms = 0 : If Ratiowms = 1 Then Ratio2wms = 85 : If Ratiowms = 2 Then Ratio2wms = 127 : If Ratiowms = 3 Then Ratio2wms = 170
   If Ratiowms = 4 Then Ratio2wms = 191 : If Ratiowms = 5 Then Ratio2wms = 212 : If Ratiowms = 6 Then Ratio2wms = 223 : If Ratiowms = 7 Then Ratio2wms = 229
   If Ratiowms = 8 Then Ratio2wms = 239 : If Ratiowms = 9 Then Ratio2wms = 242 : If Ratiowms = 10 Then Ratio2wms = 255 : If Ratiowms > 10 Then Ratio2wms = 0
End Function

'konwersja parametru Attack na wartosc nadawana do WMS
Function Attack2wms(attackwms)
   Attack2wms = Attackwms
   If Attackwms = 0 Then Attack2wms = 0 : If Attackwms = 1 Then Attack2wms = 1 : If Attackwms = 2 Then Attack2wms = 2 : If Attackwms = 3 Then Attack2wms = 5
   If Attackwms = 4 Then Attack2wms = 10 : If Attackwms = 5 Then Attack2wms = 20 : If Attackwms = 6 Then Attack2wms = 50 : If Attackwms = 7 Then Attack2wms = 100
   If Attackwms = 8 Then Attack2wms = 200 : If Attackwms = 9 Then Attack2wms = 255 : If Attackwms > 9 Then Attack2wms = 100
End Function

'konwersja parametru Release na wartosc nadawana do WMS
Function Release2wms(releasewms)
   Release2wms = Releasewms
   If Releasewms = 0 Then Release2wms = 0 : If Releasewms = 1 Then Release2wms = 1 : If Releasewms = 2 Then Release2wms = 2 : If Releasewms = 3 Then Release2wms = 5
   If Releasewms = 4 Then Release2wms = 10 : If Releasewms = 5 Then Release2wms = 20 : If Releasewms = 6 Then Release2wms = 50 : If Releasewms = 7 Then Release2wms = 100
   If Releasewms = 8 Then Release2wms = 200 : If Releasewms = 9 Then Release2wms = 255 : If Releasewms > 9 Then Release2wms = 100
End Function

'listy parametrow
'pusta
Lista(1):
Data "      "

'kluczowanie
Lista(2):
Data "  Off " , "  On  "

'threshold  agc
Lista(3):
Data "-30dB " ,"-29dB " ,"-28dB " ,"-27dB " ,"-26dB " ,"-25dB " ,"-24dB " ,"-23dB " ,"-22dB " ,"-21dB " ,"-20dB " , "-19dB " , "-18dB " , "-17dB " , "-16dB " , "-15dB " , "-14dB " , "-13dB " , "-12dB " , "-11dB " , "-10dB " , " -9dB " , " -8dB " , " -7dB " , " -6dB " , " -5dB " , " -4dB " , " -3dB " , " -2dB " , " -1dB " , "  0dB " , " +1dB " , " +2dB " , " +3dB " , " +4dB " , " +5dB " , " +6dB "

'end level
Lista(4):
Data "-15dB " , "-14dB " , "-13dB " , "-12dB " , "-11dB " , "-10dB " , " -9dB " , " -8dB " , " -7dB " , " -6dB " , " -5dB " , " -4dB " , " -3dB " , " -2dB " , " -1dB " , "  0dB " , " +1dB " , " +2dB " , " +3dB " , " +4dB " , " +5dB " , " +6dB "

'max ag
Lista(5):
Data "-10dB " , " -9dB " , " -8dB " , " -7dB " , " -6dB " , " -5dB " , " -4dB " , " -3dB " , " -2dB " , " -1dB " , "  0dB " , " +1dB " , " +2dB " , " +3dB " , " +4dB " , " +5dB " , " +6dB " , " +7dB " , " +8dB " , " +9dB " , "+10dB " , "+11dB " , "+12dB " , "+13dB " , "+14dB " , "+15dB "

'algorytm
Lista(6):
Data " self " , "other " , "  <   " , "  >   " , "Bypass"

'speedup / speeddown
Lista(7):
Data " 50ms " , " 100ms" , " 150ms" , " 200ms" , " 250ms" , " 300ms" , " 350ms" , " 400ms" , " 450ms" , " 500ms" , " 550ms" , " 600ms" , " 650ms" , " 700ms" , " 750ms" , " 800ms" , " 850ms" , " 900ms" , "1000ms"

'section
Lista(8):
Data "-infdB" , "-20dB " , "-19dB " , "-18dB " , "-17dB " , "-16dB " , "-15dB " , "-14dB " , "-13dB " , "-12dB " , "-11dB " , "-10dB " , " -9dB " , " -8dB " , " -7dB " , " -6dB " , " -5dB " , " -4dB " , " -3dB " , " -2dB " , " -1dB " , "  0dB " , " +1dB " , " +2dB " , " +3dB " , " +4dB " , " +5dB " , " +6dB " , " +7dB " , " +8dB " , " +9dB " , "+10dB " , "+11dB " , "+12dB " , "+13dB " , "+14dB " , "+15dB "

'agc ratio
Lista(9):
Data "   1  " , "   2  " , "   3  " , "   4  " , "   5  "

'filtr Lo
Lista(10):
Data " 75Hz " , "150Hz " , "250Hz " , "330Hz " , "470Hz " , "550Hz " , "660Hz " , "750Hz " , "950Hz " , "1050Hz" , "1130Hz" , "1270Hz" , "1430Hz" , "1600Hz" , "1650Hz" , "1800Hz"

'filtr Hi
Lista(11):
Data "250Hz " , "570Hz " , "950Hz " , "1250Hz" , "1750Hz" , "2000Hz" , "2500Hz" , "2800Hz" , "3100Hz" , "3500Hz" , "4000Hz" , "4200Hz" , "4500Hz" , "5000Hz" , "5300Hz" , "5600Hz"


'threshold compressor
Lista(12):
Data "-20dB " , "-19dB " , "-18dB " , "-17dB " , "-16dB " , "-15dB " , "-14dB " , "-13dB " , "-12dB " , "-11dB " , "-10dB " , " -9dB " , " -8dB " , " -7dB " , " -6dB " , " -5dB " , " -4dB " , " -3dB " , " -2dB " , " -1dB " , "  0dB "


'ratio compressor
Lista(13):
Data "  1:1 " , "1.5:1 " , "  2:1 " , "  3:1 " , "  4:1 " , "  6:1 " , "  8:1 " , " 10:1 " , " 16:1 " , " 20:1 " , "MAX:1 "

'attack compressor
Lista(14):
Data "  1ms " , "  2ms " , "  4ms " , "  8ms " , " 15ms " , " 30ms " , " 50ms " , " 80ms " , "100ms " , "200ms "

'release compressor
Lista(15):
Data "  1ms " , "  2ms " , "  4ms " , "  8ms " , " 15ms " , " 30ms " , " 50ms " , " 80ms " , "100ms " , "200ms "

'output
Lista(16):
Data "-10dB " , " -9dB " , " -8dB " , " -7dB " , " -6dB " , " -5dB " , " -4dB " , " -3dB " , " -2dB " , " -1dB " , "  0dB " , " +1dB " , " +2dB " , " +3dB " , " +4dB " , " +5dB " , " +6dB "

'compressor mode
Lista(17):
Data "  off " , " self " , "  >   "

'limiter attack
Lista(18):
Data "  1ms " , "  2ms " , "  4ms " , "  8ms " , " 15ms " , " 30ms " , " 50ms " , " 80ms " , "100ms " , "200ms "

'limiter release
Lista(19):
Data "  1ms " , "  2ms " , "  4ms " , "  8ms " , " 15ms " , " 30ms " , " 50ms " , " 80ms " , "100ms " , "200ms "

'full
Lista(20):
Data "  000 " , "  001 " , "  002 " , "  003 " , "  004 " , "  005 " , "  006 " , "  007 " , "  008 " , "  009 " , "  010 " , "  011 " , "  012 " , "  013 " , "  014 " , "  015 " , "  016 " , "  017 " , "  018 " , "  019 " , "  020 " , "  021 " , "  022 " , "  023 " , "  024 " , "  025 " , "  026 " , "  027 " , "  028 " , "  029 " , "  030 " , "  031 "

Data "  032 " , "  033 " , "  034 " , "  035 " , "  036 " , "  037 " , "  038 " , "  039 " , "  040 " , "  041 " , "  042 " , "  043 " , "  044 " , "  045 " , "  046 " , "  047 " , "  048 " , "  049 " , "  050 " , "  051 " , "  052 " , "  053 " , "  054 " , "  055 " , "  056 " , "  057 " , "  058 " , "  059 " , "  060 " , "  061 " , "  062 " , "  063 "

Data "  064 " , "  065 " , "  066 " , "  067 " , "  068 " , "  069 " , "  070 " , "  071 " , "  072 " , "  073 " , "  074 " , "  075 " , "  076 " , "  077 " , "  078 " , "  079 " , "  080 " , "  081 " , "  082 " , "  083 " , "  084 " , "  085 " , "  086 " , "  087 " , "  088 " , "  089 " , "  090 " , "  091 " , "  092 " , "  093 " , "  094 " , "  095 "

Data "  096 " , "  097 " , "  098 " , "  099 " , "  100 " , "  101 " , "  102 " , "  103 " , "  104 " , "  105 " , "  106 " , "  107 " , "  108 " , "  109 " , "  110 " , "  111 " , "  112 " , "  113 " , "  114 " , "  115 " , "  116 " , "  117 " , "  118 " , "  119 " , "  120 " , "  121 " , "  122 " , "  123 " , "  124 " , "  125 " , "  126 " , "  127 "

Data "  128 " , "  129 " , "  130 " , "  131 " , "  132 " , "  133 " , "  134 " , "  135 " , "  136 " , "  137 " , "  138 " , "  139 " , "  140 " , "  141 " , "  142 " , "  143 " , "  144 " , "  145 " , "  146 " , "  147 " , "  148 " , "  149 " , "  150 " , "  151 " , "  152 " , "  153 " , "  154 " , "  155 " , "  156 " , "  157 " , "  158 " , "  159 "

Data "  160 " , "  161 " , "  162 " , "  163 " , "  164 " , "  165 " , "  166 " , "  167 " , "  168 " , "  169 " , "  170 " , "  171 " , "  172 " , "  173 " , "  174 " , "  175 " , "  176 " , "  177 " , "  178 " , "  179 " , "  180 " , "  181 " , "  182 " , "  183 " , "  184 " , "  185 " , "  186 " , "  187 " , "  188 " , "  189 " , "  190 " , "  191 "

Data "  192 " , "  193 " , "  194 " , "  195 " , "  196 " , "  197 " , "  198 " , "  199 " , "  200 " , "  201 " , "  202 " , "  203 " , "  204 " , "  205 " , "  206 " , "  207 " , "  208 " , "  209 " , "  210 " , "  211 " , "  212 " , "  213 " , "  214 " , "  215 " , "  216 " , "  217 " , "  218 " , "  219 " , "  220 " , "  221 " , "  222 " , "  223 "

Data "  224 " , "  225 " , "  226 " , "  227 " , "  228 " , "  229 " , "  230 " , "  231 " , "  232 " , "  233 " , "  234 " , "  235 " , "  236 " , "  237 " , "  238 " , "  239 " , "  240 " , "  241 " , "  242 " , "  243 " , "  244 " , "  245 " , "  246 " , "  247 " , "  248 " , "  249 " , "  250 " , "  251 " , "  252 " , "  253 " , "  254 " , "  255 "