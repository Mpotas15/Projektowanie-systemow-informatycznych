###### ---- Zadania FUNKCJE I WYRAŻENIA WARUNKOWE ----

# 1. Stwórz funkcję o nazwie kostka, która będzie symulować n rzutów kostką.
# Wskazówka: Użyj funkcji sample() do losowania liczby oczek od 1 do 6.
kostka = function(n){
  sample(1:6, n, replace = TRUE)
}
kostka(10)

# 2. Stwórz funkcję, która będzie tworzyć wektor o zadanej długości.
# Funkcja ma zwracać wektor liczb całkowitych od 1 do n:
#  długość wektora wynosi n, a wartości w wektorze to sekwencja liczb od 1 do n.
wektor1=function(n){
  w=seq(1,n)
  return(w)
}
wektor1(10)

# 3. Stwórz funkcję o nazwie pole_kola, która oblicza pole powierzchni koła dla danego promienia.
pole_kola=function(r){
  pi*r^2
}
pole_kola(10)

# 4. Stwórz funkcję, która oblicza długość przeciwprostokątnej w trójkącie prostokątnym.
pitagoras=function(x,y){
  sqrt(x^2+y^2)
}
pitagoras(2,5)
# 5. Stwórz funkcję będącą najprostszą wersją kalkulatora 
# (dodawanie, odejmowanie, mnożenie albo dzielenie dwóch liczb).
kalkulator=function(a,operacja,b){
  if(operacja=="+"){
    a+b
  }
  else if(operacja=="-"){
    a-b
  }
  else if(operacja=="*"){
    a*b
  }
  else if(operacja=="/"){
    if(b!=0){
      a/b
    }else{
      "nie mozna wykonac"
    }
  }
}
kalkulator(10,"+",10)
kalkulator(10,"/",0)
# 6. Stwórz funkcję o nazwie przyznaj_nagrode()
# która symuluje rzut sześcienną kostką do gry i przyznaje nagrodę w zależności od wyniku rzutu. 
# Funkcja powinna działać według następujących zasad:
# - Jeśli wyrzucona liczba oczek to 6, funkcja powinna zwrócić komunikat: "Super bonus!"
# - Jeśli wyrzucona liczba oczek to 4 lub 5, funkcja powinna zwrócić komunikat: "Nagroda standardowa"
# - Jeśli wyrzucona liczba oczek to 1, 2 lub 3, funkcja powinna zwrócić komunikat: "Brak nagrody..."
przyznaj_nagrode=function(){
  x=sample(1:6,1,replace=TRUE)
  if(x==6){
    "super bonus!"
  }else if(x==5||x==4){
    "Nagroda standardowa"
  }else{
    "Brak nagrody..."
  }
}
przyznaj_nagrode()
# 7. Stwórz funkcję obliczającą podatek w zależności od dochodu. 
# Przyjmij następujące założenia:
# a) Jeżeli podatnik rozlicza się liniowo, wtedy niezależnie od kwoty płaci 19% podatku.
# b) Jeżeli podatnik rozlicza się na zasadach ogólnych, wtedy:
# - poniżej kwoty 85528zł płaci 18% podatku minus kwota zmniejszająca, czyli 556zł;
# - powyżej kwoty 85528zł płaci 14839zł + 32% nadwyżki powyżej 85528zł.
podatek=function(kwota,sposob){
  if(sposob=="liniowy"){
    kwota*0.19
  }else if(sposob=="zasady ogolne"){
    if(kwota<=85528){
      kwota*0.18-556
    }else{
      14839+(kwota-85528)*0.32
    }
  }
}
podatek(100000,"liniowy")
podatek(100000,"zasady ogolne")
