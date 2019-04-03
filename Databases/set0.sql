-- vim: set syntax=sql:
-- 1. Podaj (uporządkowane alfabetycznie, zapisane z polskimi literami i oddzielone przecinkami - bez spacji) nazwiska prowadzących ćwiczenia z Matematyki dyskretnej (M) w semestrze zimowym 2017/2018.

SELECT imie, nazwisko FROM uzytkownik
    JOIN grupa USING (kod_uz)
    JOIN przedmiot_semestr USING (kod_przed_sem)
    JOIN semestr USING (semestr_id)
    JOIN przedmiot USING (kod_przed)
WHERE przedmiot.nazwa='Matematyka dyskretna (M)'
AND grupa.rodzaj_zajec='c'
AND semestr.nazwa LIKE '%zimowy 2017%'
;


-- 2. Podaj imię i nazwisko osoby (oddzielone 1 spacją), która jako pierwsza zapisała się na wykład z Matematyki dyskretnej (M) w semestrze zimowym 2017/2018.


SELECT imie,nazwisko,data FROM uzytkownik
    JOIN wybor USING (kod_uz)
    JOIN grupa USING (kod_grupy)
    JOIN przedmiot_semestr USING (kod_przed_sem)
    JOIN semestr USING (semestr_id)
    JOIN przedmiot USING (kod_przed)
WHERE grupa.rodzaj_zajec='w'
AND przedmiot.nazwa='Matematyka dyskretna (M)'
AND semestr.nazwa LIKE '%zimowy 2017%'
ORDER BY wybor.data
LIMIT 1
;


-- 3. Przez ile dni (zaokrągl wynik w górę) studenci zapisywali się na wykład z Matematyki dyskretnej (M) w semestrze zimowym 2017/2018

SELECT b.data - a.data FROM
    (SELECT uzytkownik.kod_uz,kod_grupy,data FROM uzytkownik
        JOIN wybor USING (kod_uz)
        JOIN grupa USING (kod_grupy)
        JOIN przedmiot_semestr USING (kod_przed_sem)
        JOIN semestr USING (semestr_id)
        JOIN przedmiot USING (kod_przed)
    WHERE grupa.rodzaj_zajec='w'
    AND przedmiot.nazwa='Matematyka dyskretna (M)'
    AND semestr.nazwa LIKE '%zimowy 2017%'
    ORDER BY wybor.data ASC
    LIMIT 1) as a
JOIN
    (SELECT b.kod_uz,kod_grupy,data FROM uzytkownik b
        JOIN wybor USING (kod_uz)
        JOIN grupa USING (kod_grupy)
        JOIN przedmiot_semestr USING (kod_przed_sem)
        JOIN semestr USING (semestr_id)
        JOIN przedmiot USING (kod_przed)
    WHERE grupa.rodzaj_zajec='w'
    AND przedmiot.nazwa='Matematyka dyskretna (M)'
    AND semestr.nazwa LIKE '%zimowy 2017%'
    ORDER BY wybor.data DESC
    LIMIT 1) as b
USING (kod_grupy)
;

-- 4. Do ilu przedmiotów obowiązkowych jest repetytorium?

SELECT DISTINCT COUNT(nazwa) FROM przedmiot
    JOIN przedmiot_semestr USING(kod_przed)
    JOIN grupa USING(kod_przed_sem)
WHERE przedmiot.rodzaj='o' AND grupa.rodzaj_zajec='e'
;

-- 5. Ile osób prowadziło ćwiczenia do przedmiotów obowiązkowych w semestrach zimowych? Do odpowiedzi wliczamy sztucznych użytkowników (o “dziwnych” nazwiskach).

SELECT DISTINCT COUNT(kod_uz) FROM uzytkownik
    JOIN grupa USING(kod_uz)
    JOIN przedmiot_semestr USING(kod_przed_sem)
    JOIN przedmiot USING(kod_przed)
    JOIN semestr USING(semestr_id)
WHERE semestr.nazwa LIKE '%zimowy%'
AND przedmiot.rodzaj='o'
AND rodzaj_zajec in ('c', 'C')
;


-- 6. Podaj nazwy wszystkich przedmiotów (w kolejności alfabetycznej, oddzielone przecinkami, a wewnątrz nazw pojedyńczymi spacjami), do których zajęcia prowadził użytkownik o nazwisku Urban.

SELECT DISTINCT przedmiot.nazwa FROM uzytkownik
    JOIN grupa USING(kod_uz)
    JOIN przedmiot_semestr USING(kod_przed_sem)
    JOIN przedmiot USING(kod_przed)
WHERE nazwisko='Urban'
AND rodzaj_zajec in ('c', 'C')
;


-- 7. Ile jest w bazie osób o nazwisku Kabacki z dowolnym numerem na końcu?

SELECT COUNT(*) FROM uzytkownik
WHERE nazwisko LIKE 'Kabacki%'
;


-- 8. Ile osób co najmniej dwukrotnie się zapisało na Algorytmy i struktury danych (M) w różnych semestrach (na dowolne zajęcia)?



-- 9. W którym semestrze (podaj numer) było najmniej przedmiotów obowiązkowych (rozważ tylko semestry, gdy był co najmniej jeden)?

SELECT semestr.nazwa, COUNT(kod_przed) as przed_count FROM semestr
    JOIN przedmiot_semestr USING(semestr_id)
    JOIN przedmiot USING(kod_przed)
WHERE rodzaj='o'
GROUP BY semestr.nazwa
ORDER BY przed_count ASC
LIMIT 1
;


-- 10. Ile grup ćwiczeniowych z Logiki dla informatyków  było w semestrze zimowym  2017/2018?


SELECT COUNT(*) FROM grupa
    JOIN przedmiot_semestr USING(kod_przed_sem)
    JOIN przedmiot USING(kod_przed)
    JOIN semestr USING(semestr_id)
WHERE semestr.nazwa LIKE '%zimowy 2017%'
AND przedmiot.nazwa LIKE 'Logika dla informatyk%'
AND rodzaj_zajec in ('c', 'C')
;


-- 11. W którym semestrze (podaj numer) było najwięcej przedmiotów obowiązkowych?

SELECT semestr_id, semestr.nazwa, COUNT(kod_przed_sem) as przed_count FROM semestr
    JOIN przedmiot_semestr USING(semestr_id)
    JOIN przedmiot USING(kod_przed)
WHERE przedmiot.rodzaj='o'
GROUP BY semestr_id, semestr.nazwa
ORDER BY przed_count DESC
LIMIT 1
;

-- 12. Ile przedmiotów ma w nazwie dopisek '(ang.)'?

SELECT COUNT(*) FROM przedmiot
WHERE nazwa LIKE '%(ang.)%'
;

-- 13. W jakim okresie (od dnia do dnia) studenci zapisywali się na przedmioty w semestrze zimowym 2016/2017? Podaj odpowiedź w formacie rrrr-mm-dd,rrrr-mm-dd



-- 14. Ile przedmiotów typu kurs nie miało edycji w żadnym semestrze (nie występują w tabeli przedmiot_semestr)?

SELECT COUNT(*) FROM przedmiot
    LEFT JOIN przedmiot_semestr USING(kod_przed)
WHERE przedmiot.rodzaj='k'
AND kod_przed_sem IS NULL
;


-- 15. Ile grup ćwiczenio-pracowni prowadziła P. Kanarek?

SELECT COUNT(*) FROM grupa
    JOIN uzytkownik USING(kod_uz)
WHERE uzytkownik.nazwisko='Kanarek'
AND rodzaj_zajec in ('r', 'R')
;

-- 16. Ile grup z Logiki dla informatyków prowadził W. Charatonik?

SELECT COUNT(*) FROM grupa
    JOIN uzytkownik USING(kod_uz)
    JOIN przedmiot_semestr USING(kod_przed_sem)
    JOIN przedmiot USING(kod_przed)
WHERE uzytkownik.nazwisko='Charatonik'
AND przedmiot.nazwa LIKE 'Logika dla informat%'
AND rodzaj_zajec in ('c', 'C')
;

-- 17. Ile osób uczęszczało dwa razy na Bazy danych?

SELECT COUNT(result.kod_uz) FROM
    (SELECT u.kod_uz, COUNT(kod_grupy) as years_count FROM uzytkownik as u
        JOIN wybor USING(kod_uz)
        JOIN grupa USING(kod_grupy)
        JOIN przedmiot_semestr USING(kod_przed_sem)
        JOIN przedmiot USING(kod_przed)
    WHERE przedmiot.nazwa='Bazy danych'
    AND grupa.rodzaj_zajec='w'
    GROUP BY u.kod_uz
    HAVING COUNT(kod_grupy)='2') as result
;

-- 18. Ile osób zapisało sie na jakiś przedmiot w każdym z semestrów zapisanych w bazie?

SELECT COUNT(*) FROM
    (SELECT s.kod_uz, COUNT(s.nazwa) FROM
        (SELECT DISTINCT uzytkownik.kod_uz, semestr.nazwa FROM uzytkownik
            JOIN wybor USING(kod_uz)
            JOIN grupa USING(kod_grupy)
            JOIN przedmiot_semestr USING(kod_przed_sem)
            JOIN semestr USING(semestr_id)
        ) as s
    GROUP BY s.kod_uz
    HAVING COUNT(s.nazwa)=(SELECT COUNT(*) FROM semestr)) as s;

