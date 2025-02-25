# Zadanie 1. Analiza pojedynczego zdania ----

# Wczytaj dane tekstowe
text <- "And so even though we face the difficulties of today and tomorrow, I still have a dream."
text

# Sprawdź częstości słów za pomocą pakietu qdap
install.packages("qdap")
library(qdap)

freq_terms(text)
text
# Zapisz najczęściej występujące terminy w ramce danych
frequent_terms <- freq_terms(text)
frequent_terms

# Wizualizacja najczęściej występujących terminów
plot(frequent_terms)

# UWAGA
# Słowa nie są wymienione w takiej kolejności, w jakiej występują w zdaniu
# są prezentowane w porządku alfabetycznym.
# Takie podejście nazywa się Bag of Words (torba słów).

# Inne możliwości pakietu qdap
?freq_terms

# Wizualizacja za pomocą ggplot2
library(ggplot2)

ggplot(frequent_terms, aes(x = WORD, y = FREQ)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Słowo", y = "Częstość") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle("Wykres częstości słów")

ggplot(frequent_terms, aes(y = WORD, x = FREQ)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(x = "Słowo", y = "Częstość") +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  ggtitle("Wykres częstości słów")

# Bardziej atrakcyjna wizualizacja
ggplot(frequent_terms, aes(x = FREQ, y = reorder(WORD, FREQ))) +
  geom_bar(stat = "identity", fill = "skyblue", color = "darkblue", alpha = 0.8) +
  labs(x = "Częstość", y = "Słowo") +
  ggtitle("Wykres częstości słów") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 10), # Dostosowanie rozmiaru czcionki etykiet na osi Y
        plot.title = element_text(hjust = 0.5, size = 16, face = "bold"), # Wyśrodkowanie i stylizacja tytułu wykresu
        panel.grid.major.y = element_blank(), # Usunięcie głównych linii siatki poziomej
        panel.grid.minor.y = element_blank(), # Usunięcie mniejszych linii siatki poziomej
        axis.line = element_line(color = "black")) # Dostosowanie linii osi

# Stopwords (stop słowa – słowa do usunięcia)
# Najczęściej występujące 25, 100 i 200 słów

Top25Words
Top100Words
Top200Words

# Usunięcie stop słów
frequent_terms2 <- freq_terms(text, stopwords = Top25Words)
frequent_terms3 <- freq_terms(text, stopwords = Top100Words)
frequent_terms4 <- freq_terms(text, stopwords = Top200Words)

plot(frequent_terms4)

# Zadanie 2. Analiza całego akapitu ----

# Wczytaj dane tekstowe
text <- "And so even though we face the difficulties of today and tomorrow, I still have a dream. It is a dream deeply rooted in the American dream."
text

frequent_terms <- freq_terms(text)
frequent_terms
frequent_terms <- freq_terms(text, stopwords = Top200Words)
plot(frequent_terms)

#Potrzeba klienta
#ystem do analizy częstości tekstu, która pozwoli określić, jakie słowa i tematy dominują w pliku tekstowym (tu przemówienia Bidena z 2021 i 2024), a także wskazać różnice w sposobie przekazu i priorytetach prezydenta w danym wystąpieniu. System ma wizualizować wyniki.

#Etapy procesu tworzenia systemu informatycznego
#1.	Planowanie
#Celem analizy jest identyfikacja i porównanie najczęściej występujących słów w przemówieniu (tu prezydenta Joe Bidena). Analiza pozwoli określić dominujące tematy oraz potencjalne podobieństwa/różnice dla każdego przemówienia. 
#2.	Analiza (wymagań)
#Proces obejmie ekstrakcję tekstu, jego wstępne przetworzenie oraz analizę częstości występowania słów, a także wizualizację wyników w formie chmur słów oraz wykresów słupkowych.
#3.	Projektowanie
#Przygotowanie metod analizy tekstu i wizualizacji wyników
#1.	Wczytanie tekstu przemówień – import pliku tekstowego zawierającego przemówienie.
#2.	Przetwarzanie tekstu – oczyszczenie danych, usunięcie znaków interpunkcyjnych i konwersja tekstu do postaci tokenów.
#3.	Usunięcie stop słów – eliminacja słów o wysokiej częstości, ale niskiej wartości analitycznej (np. „i”, „oraz”, „dla”).
#4.	Analiza częstości słów – identyfikacja i porównanie najczęściej występujących terminów w obu przemówieniach.
#5.	Wizualizacja wyników:
#  o	Wykresy słupkowe – przedstawienie najczęściej używanych słów i ich liczebności.
#o	Chmury słów – graficzne zobrazowanie częstości słów, gdzie większa czcionka oznacza wyższe występowanie.
#o	Porównanie wyników – zestawienie najważniejszych różnic między przemówieniami pod kątem słownictwa i tematów.

#4.	Implementacja -wytworzenie kodu systemu

#Zadanie: rozsypanka
#Rozwiązanie (kod) nie jest uporządkowane. Zidentyfikuj kolejność i uporządkuj proces implementacji i kodoania tworzenia systemu informatycznego, a następnie uruchom kod i wykonaj analizy dla obu plików tekstowych (przemówień Bidena z 2021 i 2024). Czy priorytety wykryte w obu przemówieniach są podobne czy różnią się?

# Wczytaj dane tekstowe
# Wczytaj plik tekstowy z lokalnego dysku
text <- readLines(file.choose())
text

frequent_terms <- freq_terms(text)
frequent_terms
frequent_terms <- freq_terms(text, stopwords = Top200Words)
plot(frequent_terms)


# Tworzenie chmury słów za pomocą pakietu wordcloud
install.packages("wordcloud")
library(wordcloud)


# Utwórz chmurę słów
wordcloud(frequent_terms$WORD, frequent_terms$FREQ)

# Ograniczenie liczby słów w chmurze poprzez określenie minimalnej częstości
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4)


# Ograniczenie liczby słów w chmurze poprzez określenie maksymalnej liczby słów
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, max.words = 5)



# Opcje chmury słów
?wordcloud
# Zmiana wartości min.freq i max.words w celu wyświetlenia mniejszej/większej liczby słów.
# min.freq: słowa o częstości poniżej tej wartości nie będą wyświetlane
# max.words: maksymalna liczba słów do wyświetlenia

  
  # Dodanie różnych palet kolorystycznych
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(9,"Blues"))
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(9,"Reds"))
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(9,"Greens"))

# Optymalizacja i dostosowanie wyników
# Dodanie koloru do chmury słów dla lepszej wizualizacji
# Dodanie koloru
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, min.freq = 4, colors = brewer.pal(8,"Dark2"))
# Dodanie koloru
wordcloud(frequent_terms$WORD, frequent_terms$FREQ, max.words = 5, colors = brewer.pal(8,"Accent"))
?brewer.pal
brewer.pal.info
