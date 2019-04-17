-- 1. Podaj kody, imiona i nazwiska wszystkich osób, które chodziły na dowolne zajęcia z Algorytmów
-- i struktur danych, a w jakimś semestrze późniejszym (o większym numerze) chodziły na zajęcia z
-- Matematyki dyskretnej. Za AiSD oraz MD uznaj wszystkie przedmioty, których nazwa zaczyna się od
-- podanych nazw. Zapisz to zapytanie używając operatora IN z podzapytaniem.

SELECT DISTINCT kod_uz, imie, nazwisko FROM
    (SELECT DISTINCT u.kod_uz, imie, nazwisko, semestr_id as aisd_semestr FROM uzytkownik as u
    JOIN wybor USING(kod_uz)
    JOIN grupa USING(kod_grupy)
    JOIN przedmiot_semestr USING(kod_przed_sem)
    JOIN przedmiot USING(kod_przed)
    JOIN semestr USING(semestr_id)
    WHERE przedmiot.nazwa LIKE 'Algorytmy i struktury danych%'
    ) as aisd
JOIN
    (SELECT DISTINCT u.kod_uz, imie, nazwisko, semestr_id as mdm_semestr FROM uzytkownik as u
    JOIN wybor USING(kod_uz)
    JOIN grupa USING(kod_grupy)
    JOIN przedmiot_semestr USING(kod_przed_sem)
    JOIN przedmiot USING(kod_przed)
    WHERE przedmiot.nazwa LIKE 'Matematyka dyskretna%'
    ) as mdm
USING(kod_uz, imie, nazwisko)
WHERE aisd_semestr < mdm_semestr
;

-- 2. Zapisz zapytanie pierwsze używając operatora EXISTS z podzapytaniem.

SELECT DISTINCT u.kod_uz, imie, nazwisko FROM uzytkownik as u
JOIN wybor USING(kod_uz)
JOIN grupa USING(kod_grupy)
JOIN przedmiot_semestr USING(kod_przed_sem)
JOIN przedmiot USING(kod_przed)
JOIN semestr aisd_semestr USING(semestr_id)
WHERE przedmiot.nazwa LIKE 'Algorytmy i struktury danych%'
AND EXISTS(
    SELECT DISTINCT * FROM semestr as mdm_sem
        JOIN przedmiot_semestr USING(semestr_id)
        JOIN przedmiot USING(kod_przed)
        JOIN grupa USING(kod_przed_sem)
        JOIN wybor USING(kod_grupy)
        JOIN uzytkownik u2 ON(wybor.kod_uz=uzytkownik.kod_uz)
        WHERE przedmiot.nazwa LIKE 'Matematyka dyskretna%'
        AND mdm_sem.semestr_id > aisd_semestr.semestr_id
        AND u.kod_uz = u2.kod_uz
)
;

-- 3. Podaj kody, imiona i nazwiska osób, które prowadziły jakiś wykład, ale nigdy
-- nie prowadziły żadnego seminarium (nie patrzymy, czy zajęcia były w tym samym
-- semestrze). Pisząc zapytanie użyj operatora NOT EXISTS.

SELECT DISTINCT kod_uz, imie, nazwisko FROM uzytkownik as u
JOIN grupa USING(kod_uz)
WHERE rodzaj_zajec='w'
AND NOT EXISTS(
    SELECT * FROM uzytkownik as u2
    JOIN grupa USING(kod_uz)
    WHERE rodzaj_zajec='s'
    AND u.kod_uz=u2.kod_uz
)
;

-- 4. Zapisz zapytanie trzecie, używając różnicy zbiorów.

(SELECT kod_uz, imie, nazwisko FROM uzytkownik as u
    JOIN grupa USING(kod_uz)
    WHERE rodzaj_zajec='w')
EXCEPT
(SELECT kod_uz, imie, nazwisko FROM uzytkownik as u2
    JOIN grupa USING(kod_uz)
    WHERE rodzaj_zajec='s')
;

-- 5. Dla każdego przedmiotu typu kurs z bazy danych podaj jego nazwę oraz
-- liczbę osób, które na niego uczęszczały. Uwzględnij w odpowiedzi kursy,
-- na które nikt nie uczęszczał – w tym celu użyj złączenia zewnętrznego
-- (LEFT JOIN lub RIGHT JOIN).


SELECT przedmiot.nazwa, COUNT(DISTINCT wybor.kod_uz) FROM przedmiot
LEFT JOIN przedmiot_semestr USING(kod_przed)
LEFT JOIN grupa USING(kod_przed_sem)
LEFT JOIN wybor USING(kod_grupy)
WHERE rodzaj='k'
GROUP BY przedmiot.kod_przed, przedmiot.nazwa
;


-- 6. Podaj kody użytkowników, którzy uczęszczali w semestrze letnim 2016/2017
-- na wykład z 'Baz danych' i nie uczęszczali na wykład z 'Sieci komputerowych',
-- i odwrotnie. Sformułuj to zapytanie używając instrukcji WITH, by wstępnie
-- zdefiniować zbiory osób uczęszczających na każdy z wykładów.

-- 7. Podaj kody, imiona i nazwiska wszystkich prowadzących, którzy w jakiejś prowadzonej przez siebie grupie mieli więcej zapisanych osób, niż wynosił limit max_osoby dla tej grupy. Do zapisania zapytania użyj GROUP BY i HAVING.

-- 8. Podaj nazwę przedmiotu podstawowego, na wykład do którego chodziło najwięcej różnych osób. Użyj w tym celu zapytania z GROUP BY i HAVING (z warunkiem używającym ponownie GROUP BY).

-- 9. Dla każdego semestru letniego podaj jego numer oraz nazwisko osoby, która jako pierwsza zapisała się na zajęcia w tym semestrze. Jeśli w semestrze było kilka osób, które zapisały się jednocześnie:
-- 9.1 Podaj wszystkie;
-- 9.2 Podaj tę o najwcześniejszym leksykograficznie nazwisku.

-- 10. Jaka jest średnia liczba osób zapisujących się na wykład w semestrze letnim 2016/2017? Zapisz to zapytanie definiując najpierw pomocniczą relację (np. na liście from z aliasem), w której dla każdego interesującego cię wykładu znajdziesz liczbę zapisanych na niego osób).

-- 11. Kto prowadzi w jednym semestrze wykład do przedmiotu i co najmniej dwie grupy innych zajęć do tego przedmiotu (nie muszą być tego samego typu)?

