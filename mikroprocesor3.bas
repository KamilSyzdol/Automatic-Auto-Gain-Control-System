'28.01.2024
'uC nr 3

$regfile = "m8adef.dat"
$crystal = 8000000                                          ' used crystal frequency

$hwstack = 52                                               ' default use 32 for the hardware stack
$swstack = 30                                               ' default use 10 for the SW stack
$framesize = 60                                             ' default use 40 for the frame space

'konfiguracja wyswietlacza
Config Portb.7 = Output
Portb.7 = 0
Config Lcdpin = Pin , Db4 = Portb.6 , Db5 = Portd.3 , Db6 = Portb.2 , Db7 = Portd.4 , E = Portd.1 , Rs = Portd.0 , Wr = Portb.7
Config Lcd = 40 * 2                                         'configure lcd screen
Cls                                                         'clear the LCD display
Cursor Off


Declare Sub Ram_page_write(byval Ram_page_write_param As Byte , Byval Ram_page_write_adr As Byte , Byval Ram_page_write_ile As Byte)
Declare Sub Ram_page_read(byval Ram_page_read_param As Byte , Byval Ram_page_read_adr As Byte , Byval Ram_page_read_ile As Byte)

Declare Sub Ram_write_sreg()
Declare Sub Ram_read_sreg()

Declare Sub Main_program()


Dim Przerwanie_flag As Byte


Dim Cykl As Byte

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
Config Portb.3 = Output

'adc
Declare Function Adc2db(byval Adc2db_dbl As Word) As Byte
Declare Function Adc2gain(byval Adc2gain_gainl As Word) As Byte


Declare Function Pomiar_bajt(byval Pomiar_bajt_kanal As Byte) As Byte
Declare Function Pomiar_gain(byval Pomiar_gain_kanal As Byte) As Byte
Declare Function Pomiar_db(byval Pomiar_db_kanal As Byte) As Byte
Config Adc = Single , Prescaler = Auto , Reference = Internal
Start Adc


'konfiguracja sterowania multiplekserem
Mux_adr_a Alias Portb.5
Mux_adr_b Alias Portb.4
Config Mux_adr_a = Output
Config Mux_adr_b = Output

'token-ring
Busy_out Alias Portb.1
Config Busy_out = Output


'tablica parametrow odczytanych z zewnetrznej pamieci RAM
Dim Parametr(120) As Byte
Dim Parametr_odczyt(80) As Byte

'przerwanie z int0
Config Portd.2 = Input
Portd.2 = 1
Enable Int0
Config Int0 = Falling
Enable Interrupts
On Int0 Poczatek


'zapelnianie wyswietlacza spacjami
Dim I As Byte
For I = 1 To 81
   Parametr(i) = 32
Next I

Dim Lcd_txt As String * 20

Dim Lcd_x As Byte
Dim Lcd_y As Byte

Dim Lcd_petla_start As Byte
Dim Lcd_petla_stop As Byte


'diagnostycznie
      For I = 81 To 121
      Parametr(i) = I - 80
      Next I

      Waitms 1

   Cykl = 0

   Do


   Loop




Poczatek:
Przerwanie_flag = 1
Call Main_program()

Return


Sub Main_program()

   If Przerwanie_flag = 1 Then

         Busy_out = 1                                       'token ring

         'nadanie znaku rozpoczecia cyklu - diagnostyka
         Portb.3 = 0
         Waitus 200
         Portb.3 = 1

         'odczyt z pamieci RAM danych do wyswietlenia na LCD
         Call Ram_page_read(1 , 0 , 80)
         Waitus 5

         'diagnostyczne
         For I = 81 To 120
            Parametr(i) = Parametr(i) + 1
         Next I

         'zapisywanie do pamieci RAM wynikow pomiaru
         Call Ram_page_write(81 , 160 , 40)
         Busy_out = 0                                       'token ring


         Dim Pomiar_byte As Byte
         Dim Pomiar_word As Word


         'pomiary z ADC
         Mux_adr_a = 0 : Mux_adr_b = 0
         Waitus 300
         '00 SV3 (z p³ytki sumatora wyjscia poszczególnych kompresorów)
         Pomiar_byte = Pomiar_db(0) : Parametr(81) = Pomiar_byte       'srodek prawy
         Pomiar_byte = Pomiar_db(1) : Parametr(82) = Pomiar_byte       'gora prawy
         Pomiar_byte = Pomiar_db(2) : Parametr(83) = Pomiar_byte       'gora lewy
         Pomiar_byte = Pomiar_db(3) : Parametr(84) = Pomiar_byte       'bas prawy
         Pomiar_byte = Pomiar_db(4) : Parametr(85) = Pomiar_byte       'bas lewy
         Pomiar_byte = Pomiar_db(5) : Parametr(86) = Pomiar_byte       'srodek lewy


         Mux_adr_a = 0 : Mux_adr_b = 1
         Waitus 300
         '01 SV4 (napiecie sterujace limiterem i zasilania +-15V)
         Pomiar_byte = Pomiar_gain(0) : Parametr(87) = Pomiar_byte       'vca limitera (sprawdzic L/R)
         Pomiar_byte = Pomiar_gain(1) : Parametr(88) = Pomiar_byte       'vca limitera (sprawdzic L/R)
         Pomiar_byte = Pomiar_bajt(2) : Parametr(89) = Pomiar_byte       '15V (sprawdzic + czy -)
         Pomiar_byte = Pomiar_bajt(3) : Parametr(90) = Pomiar_byte       '15V (sprawdzic + czy -)
         Pomiar_byte = Pomiar_gain(4) : Parametr(91) = Pomiar_byte       'pusty
         Pomiar_byte = Pomiar_gain(5) : Parametr(92) = Pomiar_byte       'pusty


         Mux_adr_a = 1 : Mux_adr_b = 0
         Waitus 300
         '10 SV2 (sygna³y napiêcia sterujacego VCA kompresorow)
         Pomiar_word = Getadc(0) : Pomiar_byte = Pomiar_word / 1 : Parametr(93) = Pomiar_byte
         Pomiar_word = Getadc(1) : Pomiar_byte = Pomiar_word / 1 : Parametr(94) = Pomiar_byte
         Pomiar_word = Getadc(2) : Pomiar_byte = Pomiar_word / 1 : Parametr(95) = Pomiar_byte
         Pomiar_word = Getadc(3) : Pomiar_byte = Pomiar_word / 1 : Parametr(96) = Pomiar_byte
         Pomiar_word = Getadc(4) : Pomiar_byte = Pomiar_word / 1 : Parametr(97) = Pomiar_byte
         Pomiar_word = Getadc(5) : Pomiar_byte = Pomiar_word / 1 : Parametr(98) = Pomiar_byte


         Mux_adr_a = 1 : Mux_adr_b = 1
         Waitus 300
         '11 SV1 (z plytki z gotowymi sygnalami od LIM_AR)
         Pomiar_byte = Pomiar_db(0) : Parametr(99) = Pomiar_byte       'wejscie J3 LIM_AR
         Pomiar_byte = Pomiar_db(1) : Parametr(100) = Pomiar_byte       'wejscie J4 LIM_AR
         Pomiar_byte = Pomiar_db(2) : Parametr(101) = Pomiar_byte       'lewa suma po kompresorze
         Pomiar_byte = Pomiar_db(3) : Parametr(102) = Pomiar_byte       'prawa suma po kompresorze
         Pomiar_byte = Pomiar_db(4) : Parametr(103) = Pomiar_byte       'wejscie J11 LIM_AR (wyjscie sygnalu)
         Pomiar_byte = Pomiar_db(5) : Parametr(104) = Pomiar_byte       'wejscie J12 LIM_AR (wyjscie sygnalu)


            'z kazdym cyklem aktualizowana jest tylko 1/4 wyswietlacza, poniewaz zbyt dlugo to trwa zeby zmiescic sie w 50ms
            If Cykl = 0 Then
               Lcd_x = 1
               Lcd_y = 1
               Lcd_petla_start = 1
            End If

            If Cykl = 1 Then
               Lcd_x = 21
               Lcd_y = 1
               Lcd_petla_start = 21
            End If

            If Cykl = 2 Then
               Lcd_x = 1
               Lcd_y = 2
               Lcd_petla_start = 41
            End If

            If Cykl = 3 Then
               Lcd_x = 21
               Lcd_y = 2
               Lcd_petla_start = 61
            End If

            Lcd_petla_stop = Lcd_petla_start + 19

            Lcd_txt = ""                                    'czyszczenie zmiennej

            'generowanie z pamieci RAM tekstu do wyswietlenia
            For I = Lcd_petla_start To Lcd_petla_stop
               Lcd_txt = Lcd_txt + Chr(parametr_odczyt(i))
            Next I


            'wskazanie wspolrzednych i wyswietlenie
            Locate Lcd_y , Lcd_x
            Lcd Lcd_txt


            Cykl = Cykl + 1
            If Cykl >= 4 Then Cykl = 0

            'informacja o zakonczeniu zadania i oczekiwanie na kolejne przerwanie
            Przerwanie_flag = 0

         'nadanie znaku zakonczenia cyklu - diagnostycznie
         Portb.3 = 0
         Waitus 200
         Portb.3 = 1
         Waitus 200
         Portb.3 = 0
         Waitus 200
         Portb.3 = 1




   Else

   End If



End Sub


'zapis do pamieci RAM
'ktory blok parametrow lokalnych; pierwszy adres z zewnetrznej pamieci; ilosc bajtow do nadania
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




'ktory blok parametrow lokalnych; pierwszy adres z zewnetrznej pamieci, ile bajtow do odczytania
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


   'nadawanie adresu poczatku strony (16-bit, starsza czesc) (00000001, od 256)
         Ram_page_read_wyslac = 1
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


'zapis do status register RAM
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

'odczyt status register RAM
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

'pomiar z ADC - wynik w dB
Function Pomiar_db(byval Pomiar_db_kanal As Byte)
   Dim Adc_lin_db As Word                                   'wynik pomiaru bezposrednio z ADC

   Adc_lin_db = Getadc(pomiar_db_kanal)
   Adc_lin_db = Getadc(pomiar_db_kanal)                     'to moze bedzie dalo sie usunac, poki co dla zwiekszenia stabilnosci pomiaru

   Pomiar_db = Adc_lin_db
   Pomiar_db = Adc2db(adc_lin_db)

End Function


'pomiar z ADC - wynik liniowy
Function Pomiar_bajt(byval Pomiar_bajt_kanal As Byte)

   Dim Adc_lin_bajt As Word                                 'wynik pomiaru bezposrednio z ADC

   Adc_lin_bajt = Getadc(pomiar_bajt_kanal)
   Adc_lin_bajt = Getadc(pomiar_bajt_kanal)                 'to moze bedzie dalo sie usunac, poki co dla zwiekszenia stabilnosci pomiaru
   Adc_lin_bajt = Adc_lin_bajt / 4
   Pomiar_bajt = Adc_lin_bajt
End Function

'pomiar z ADC - wynik wyskalowany we wzmocnieniu
Function Pomiar_gain(byval Pomiar_gain_kanal As Byte)

   Dim Adc_lin_gain As Word                                 'wynik pomiaru bezposrednio z ADC

   Adc_lin_gain = Getadc(pomiar_gain_kanal)
   Adc_lin_gain = Getadc(pomiar_gain_kanal)                 'to moze bedzie dalo sie usunac, poki co dla zwiekszenia stabilnosci pomiaru

   Pomiar_gain = Adc2gain(adc_lin_gain)

End Function


'tablica konwersji
Function Adc2gain(adc2gain_gainl)
  Dim Adc2gain_b As Byte
  Dim Adc2gain_c As Byte
  Dim Adc2gain_db_tab(26) As Word
  Adc2gain_b = 0
  Adc2gain_c = 0

  Adc2gain_db_tab(1) = 56
  Adc2gain_db_tab(2) = 106                                  '-30db
  Adc2gain_db_tab(3) = 151
  Adc2gain_db_tab(4) = 190
  Adc2gain_db_tab(5) = 226
  Adc2gain_db_tab(6) = 257
  Adc2gain_db_tab(7) = 285
  Adc2gain_db_tab(8) = 310
  Adc2gain_db_tab(9) = 333
  Adc2gain_db_tab(10) = 353
  Adc2gain_db_tab(11) = 370
  Adc2gain_db_tab(12) = 386                                 '-20db
  Adc2gain_db_tab(13) = 400
  Adc2gain_db_tab(14) = 413
  Adc2gain_db_tab(15) = 424
  Adc2gain_db_tab(16) = 434
  Adc2gain_db_tab(17) = 443
  Adc2gain_db_tab(18) = 451
  Adc2gain_db_tab(19) = 458
  Adc2gain_db_tab(20) = 464
  Adc2gain_db_tab(21) = 470
  Adc2gain_db_tab(22) = 475                                 '-10db
  Adc2gain_db_tab(23) = 479
  Adc2gain_db_tab(24) = 483
  Adc2gain_db_tab(25) = 487
                                  '+9db
  Adc2gain_db_tab(26) = 65535

  Adc2gain = 0

  'dziele skan na pol i od razu przenosze do wlasciwej polowki dla optymalizacji szybkosci kodu
  If Adc2gain_gainl >= Adc2gain_db_tab(13) Then
    For Adc2gain_b = 13 To 26
       If Adc2gain_gainl >= Adc2gain_db_tab(adc2gain_b) And Adc2gain_gainl < Adc2gain_db_tab(adc2gain_b + 1) Then Adc2gain_c = Adc2gain_b
    Next Adc2gain_b
  Else
     For Adc2gain_b = 1 To 12
        If Adc2gain_gainl >= Adc2gain_db_tab(adc2gain_b) And Adc2gain_gainl < Adc2gain_db_tab(adc2gain_b + 1) Then Adc2gain_c = Adc2gain_b
     Next Adc2gain_b
  End If


  Adc2gain = 63 - Adc2gain_c

End Function

'tablica konwersji
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