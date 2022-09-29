-- creates the cjdate sample database in SQL for comparison with TutorialD
BEGIN;
CREATE TABLE s (
  "s#"    varchar(2)   primary key,
  sname   varchar(10)  NOT NULL,
  status  int          NOT NULL,
  city    varchar(10)  NOT NULL
);

CREATE TABLE p (
  "p#"    varchar(2)   primary key,
  pname   varchar(10)  NOT NULL,
  color   varchar(10)  NOT NULL,
  weight  real         NOT NULL,
  city    varchar(10)  NOT NULL
);

CREATE TABLE sp (
  "s#"   varchar(2)   NOT NULL REFERENCES s("s#"),
  "p#"   varchar(2)   NOT NULL REFERENCES p("p#"),
  qty    int          NOT NULL,
  PRIMARY KEY ("s#", "p#")
);

INSERT INTO s(city, "s#", sname, status) VALUES ('Athens','S5','Adams',30),('Paris','S3','Blake',30),('London','S1','Smith',20),('Paris','S2','Jones',10),('London','S4','Clark',20);

INSERT INTO p(city,color,"p#",pname,weight) VALUES ('London','Red','P6','Cog',19),('Oslo','Blue','P3','Screw',17),('London','Red','P4','Screw',14),('London','Red','P1','Nut',12),('Paris','Green','P2','Bolt',17),('Paris','Blue','P5','Cam',12);

INSERT INTO sp("p#",qty,"s#") VALUES ('P1',300,'S2'),('P1',300,'S1'),('P2',400,'S2'),('P2',200,'S4'),('P2',200,'S3'),('P2',200,'S1'),('P3',400,'S1'),('P4',200,'S1'),('P4',300,'S4'),('P5',400,'S4'),('P5',100,'S1'),('P6',100,'S1');
COMMIT;