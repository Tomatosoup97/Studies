-- Zadanie 1
-- 1.1

CREATE DOMAIN semestry AS text
CHECK (VALUE IN ('zimowy', 'letni')) NOT NULL;

-- 1.2

CREATE SEQUENCE numer_semestru;

SELECT setval('numer_semestru', max(semestr_id)) FROM semestr;

ALTER TABLE semestr ALTER COLUMN semestr_id SET DEFAULT nextval('numer_semestru');
ALTER SEQUENCE numer_semestru OWNED BY semestr.semestr_id;

-- 1.3

ALTER TABLE semestr ADD COLUMN semestr semestry DEFAULT 'letni';

ALTER TABLE semestr ADD COLUMN rok char(9);

-- 1.4

UPDATE semestr SET semestr='zimowy' WHERE nazwa LIKE '%zimowy%';
UPDATE semestr SET semestr='letni' WHERE nazwa LIKE '%letni%';


UPDATE semestr SET rok=substring(nazwa FROM position('/' IN nazwa)-4 FOR 9);

-- 1.5

ALTER TABLE semestr DROP COLUMN nazwa;

-- 1.6

ALTER TABLE semestr ALTER COLUMN semestr DROP DEFAULT;

ALTER TABLE semestr ALTER COLUMN semestr SET DEFAULT
    CASE WHEN EXTRACT(month FROM current_date) BETWEEN 1 AND 6
        THEN 'letni' ELSE 'zimowy'
    END;

ALTER TABLE semestr ALTER COLUMN rok SET DEFAULT
    CASE WHEN EXTRACT(month FROM current_date) BETWEEN 1 AND 6
        THEN EXTRACT(year FROM current_date)-1 || '/' || EXTRACT(year FROM current_date)
        ELSE EXTRACT(year FROM current_date) || '/' || EXTRACT(year FROM current_date)+1
    END;


-- Zadanie 2

INSERT INTO semestr(semestr, rok) VALUES
    ('zimowy', '2018/2019'),
    ('letni', '2018/2019')
;

-- 2.1


CREATE SEQUENCE numer_przedmiot_semestr;
CREATE SEQUENCE numer_grupy;

SELECT setval('numer_przedmiot_semestr', max(kod_przed_sem)) FROM przedmiot_semestr;
SELECT setval('numer_grupy', max(kod_grupy)) FROM grupa;


ALTER TABLE przedmiot_semestr ALTER COLUMN kod_przed_sem SET DEFAULT nextval('numer_przedmiot_semestr');
ALTER TABLE grupa ALTER COLUMN kod_grupy SET DEFAULT nextval('numer_grupy');

ALTER SEQUENCE numer_przedmiot_semestr OWNED BY przedmiot_semestr.kod_przed_sem;
ALTER SEQUENCE numer_grupy OWNED BY grupa.kod_grupy;

-- 2.2


INSERT INTO przedmiot_semestr(semestr_id, kod_przed, strona_domowa, angielski)
SELECT s1.semestr_id, p.kod_przed, strona_domowa, angielski
FROM semestr s1, przedmiot p
JOIN przedmiot_semestr ps USING(kod_przed)
JOIN semestr s2 USING(semestr_id)
WHERE rodzaj IN ('p', 'o')
AND s2.rok='2016/2017'
AND s2.semestr=s1.semestr
AND s1.rok='2018/2019';
;


-- 2.3


INSERT INTO grupa(kod_przed_sem, max_osoby, rodzaj_zajec)
SELECT kod_przed_sem,100,'w'
FROM przedmiot_semestr
JOIN semestr USING(semestr_id)
WHERE rok='2018/2019';
;


-- Zadanie 3

-- 3.1

CREATE TABLE pracownik (LIKE uzytkownik);
ALTER TABLE pracownik DROP COLUMN semestr;
ALTER TABLE pracownik ADD CONSTRAINT pk_pracownik PRIMARY KEY(kod_uz);

CREATE TABLE student (LIKE uzytkownik);
ALTER TABLE student ADD CONSTRAINT pk_student PRIMARY KEY(kod_uz);

-- 3.2

INSERT INTO pracownik(kod_uz, imie, nazwisko)
SELECT kod_uz, imie, nazwisko FROM uzytkownik
WHERE kod_uz IN (SELECT kod_uz FROM grupa)
;

-- 3.3

INSERT INTO student(kod_uz, imie, nazwisko, semestr)
SELECT kod_uz, imie, nazwisko, semestr FROM uzytkownik
WHERE kod_uz IN (SELECT kod_uz FROM wybor)
;

-- 3.4

ALTER TABLE wybor DROP CONSTRAINT fk_wybor_uz;
ALTER TABLE wybor ADD CONSTRAINT fk_wybor_st
FOREIGN KEY (kod_uz) REFERENCES student(kod_uz) DEFERRABLE;

ALTER TABLE grupa DROP CONSTRAINT fk_grupa_uz;
ALTER TABLE grupa ADD CONSTRAINT fk_grupa_pr
FOREIGN KEY (kod_uz) REFERENCES pracownik(kod_uz) DEFERRABLE;

DROP TABLE uzytkownik;


-- Zadanie 4

-- 4.1

CREATE DOMAIN rodzaje_zajec AS char(1)
CHECK (VALUE IN ('w','s','r','p','P','e','c','C','l','g'))
NOT NULL;

-- 4.2

ALTER TABLE grupa ALTER COLUMN rodzaj_zajec TYPE rodzaje_zajec;

-- 4.3


CREATE VIEW obsada_zajec_view
    (prac_kod, prac_nazwisko, przed_kod, przed_nazwa,
     rodzaj_zajec, liczba_grup, liczba_studentow)
AS
SELECT
    prac.kod_uz, prac.nazwisko, przedmiot.kod_przed, przedmiot.nazwa,
    grupa.rodzaj_zajec, COUNT(DISTINCT kod_grupy), COUNT(wybor.kod_uz)
FROM pracownik prac
JOIN grupa USING(kod_uz)
JOIN przedmiot_semestr USING(kod_przed_sem)
JOIN przedmiot USING(kod_przed)
JOIN wybor USING(kod_grupy)
GROUP BY
    prac.kod_uz, prac.nazwisko, przedmiot.kod_przed, przedmiot.nazwa,
    grupa.rodzaj_zajec
;


CREATE TABLE obsada_zajec_tab(
   prac_kod int,
   prac_nazwisko text,
   przed_kod int,
   przed_nazwa text,
   rodzaj_zajec rodzaje_zajec,
   liczba_grup int,
   liczba_studentow int);

INSERT INTO obsada_zajec_tab SELECT * FROM obsada_zajec_view;





