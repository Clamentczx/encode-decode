-- Do not alter the following line
module Assignment1 (char_to_int, repeat_char, decode, int_to_char, length_char, drop_char, encode, complex_encode, complex_decode) where


-- Part A

char_to_int :: Char -> Integer
char_to_int '0'=0
char_to_int '1'=1
char_to_int '2'=2
char_to_int '3'=3
char_to_int '4'=4
char_to_int '5'=5
char_to_int '6'=6
char_to_int '7'=7
char_to_int '8'=8
char_to_int '9'=9


repeat_char :: Char -> Integer -> String
repeat_char c 0=""
repeat_char c n=[c] ++ repeat_char c (n-1)


decode :: String -> String
decode ""=""
decode (x:y:xs) =repeat_char x (char_to_int (y))++ decode xs


-- Part B

int_to_char :: Integer -> Char
int_to_char 0='0'
int_to_char 1='1'
int_to_char 2='2'
int_to_char 3='3'
int_to_char 4='4'
int_to_char 5='5'
int_to_char 6='6'
int_to_char 7='7'
int_to_char 8='8'
int_to_char 9='9'


length_char :: Char -> String -> Integer
length_char c ""=0
length_char c (x:xs)=if c==x then (1+length_char c (xs)) else length_char c ""


drop_char :: Char -> String -> String
drop_char c ""=""
drop_char c (x:xs)=if c==x then (""++drop_char c (xs)) else (x:xs)


encode :: String -> String
encode ""=""
encode [x]=[x]++"1"
encode (x:y:xs)=
       if x==y then [x]++[int_to_char (length_char x (x:y:xs))] ++encode (drop_char x (x:y:xs))
       else [x]++"1"++encode (y:xs)


-- Part C

complex_encode :: String -> String
length_int 0=0
length_int n=if n `div` 10 >0 then 1+length_int (n `div` 10) else length_int 0  
s n l =if n>0 then [int_to_char (n `div` (10^l))]++(s (n `mod` (10^l)) (l-1)) else if l>=0 && n==0 then "0"++ s n (l-1) else ""
complex_encode ""=""
complex_encode [x]=[x]
complex_encode (x:y:xs)=
       if x==y then [x]++s (length_char x (x:y:xs))  (length_int (length_char x (x:y:xs)))++complex_encode (drop_char x (x:y:xs))
       else [x]++complex_encode (y:xs)


complex_decode :: String -> String
string_int ""=0
string_int (x:xs)=10^(length(x:xs)-1)*char_to_int x +string_int xs
take_integer ""=""
take_integer (y:xs)= if y =='0' || y =='1' || y =='2' || y =='3' || y =='4' || y =='5' || y =='6' || y =='7'|| y == '8'|| y == '9'  then [y]++take_integer xs else take_integer ""
complex_decode ""=""
complex_decode (x:xs)=if take_integer xs/="" then repeat_char x (string_int (take_integer xs))++complex_decode (drop (length(take_integer xs)) xs) else repeat_char x 1 ++complex_decode xs
