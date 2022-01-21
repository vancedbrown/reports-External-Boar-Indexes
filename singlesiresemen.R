library(tidyverse)
library(knitr)
library(dplyr)
library(lubridate)
library(stringr)
library(DT)
library(readr)
library(here)
library(kableExtra)
library(gridExtra)
library(ggpubr)
library(lme4)
library(effects)

source('C:/Users/vance/Documents/myR/functions/getSQL.r')


query1<-"
     DECLARE @Date1 SMALLDATETIME
     DECLARE @Date2 SMALLDATETIME
     
     SET @Date1 = (SELECT Start_Date FROM Intranet..Global_Weeks WHERE DateDiff(d,365,GetDate()) BETWEEN Start_Date AND End_Date)
     SET @Date2 = (SELECT End_Date FROM Intranet..Global_Weeks WHERE DateDiff(d,7,GetDate()) BETWEEN Start_Date AND End_Date)
          
     SELECT StudID, Dest, Batch, [Day], Name, BoarID, Breed, SUM(Doses) AS Doses, CurTarget, PrevWkTarget, Day1, Day6, Idx, SireID
       FROM 
	 (SELECT DISTINCT B.StudID, 
     CASE
       WHEN Accounting='74411' THEN 'Dogwood'
       WHEN Accounting='74401' THEN 'FoxRidge'
       WHEN Accounting='5074' THEN 'W4'
       WHEN Dest='W4' THEN 'W4'
     ELSE NULL
     END AS 
     Dest,
     A.Batch,
     CASE
     WHEN DATEPART(dw,Date_Shipped) = 1 THEN '7-Sunday'
     WHEN DATEPART(dw,Date_Shipped) = 2 THEN '1-Monday'
     WHEN DATEPART(dw,Date_Shipped) = 3 THEN '2-Tuesday'
     WHEN DATEPART(dw,Date_Shipped) = 4 THEN '3-Wednesday'
     WHEN DATEPART(dw,Date_Shipped) = 5 THEN '4-Thursday'
     WHEN DATEPART(dw,Date_Shipped) = 6 THEN '5-Friday'
     WHEN DATEPART(dw,Date_Shipped) = 7 THEN '6-Saturday'
     ELSE NULL
     END AS [Day],
     B.Name,
     B.BoarID,
     B.Breed,
     D.Doses,
     E.[Target] AS CurTarget,
     E2.[Target] AS PrevWkTarget,
     DateAdd(d,1,Date_Shipped) AS Day1,
     DateAdd(d,5,Date_Shipped) AS Day6,
     DV_Idx AS Idx,
     G.SireID
     
     FROM Boar_Pig B
     INNER JOIN Boar_Split C ON B.BoarID = C.BoarID AND B.StudID = C.StudID
     INNER JOIN Boar_Distrib D ON C.BatchNum = D.BatchNum AND C.StudID = D.StudID
     INNER JOIN Intranet..Global_Weeks A ON D.Date_Shipped BETWEEN A.Start_Date AND A.End_Date
     LEFT OUTER JOIN Intranet..Boar_NonPooled E ON B.Name = E.Tattoo AND B.StudID=E.StudID  AND
     (E.SelDate <= D.Date_Shipped  AND (E.UnSelDate IS NULL OR D.Date_Shipped <= E.UnSelDate))
     LEFT OUTER JOIN Intranet..Boar_NonPooled E2 ON B.Name = E2.Tattoo AND B.StudID=E2.StudID  AND
     (E2.SelDate <= DATEDIFF(d,7,D.Date_Shipped)  AND (E2.UnSelDate IS NULL OR DATEDIFF(d,7,D.Date_Shipped) <= E2.UnSelDate))
     LEFT OUTER JOIN Intranet..ProductIdx F ON B.Name=F.SPGid AND F.Product LIKE '%PIC%'
     LEFT OUTER JOIN BlupDB.Final.Pedigree G ON B.Name=G.SPGid
     WHERE  B.StudID IN ('SPGNC','SPGVA','MB 7081', 'MB 7082')
     AND (Accounting IN ('74411','74401','5074') OR (D.StudID='MB 7082' AND D.Dest='W4'))
     AND Date_Shipped >= '11/30/2020'
     AND Date_Shipped between @Date1 and @Date2
     AND B.Breed LIKE 'PIC%'
     AND C.BatchNum IN (SELECT BatchNum FROM Boar_Split S2 WHERE C.BatchNum = S2.BatchNum AND C.StudID = S2.StudID GROUP BY BatchNum HAVING COUNT(*) = 1)
     ) AS A
     GROUP BY StudID, Dest, Batch, [Day], Name, BoarID, Breed, CurTarget, PrevWkTarget, Day1, Day6, Idx, SireID "

ext1<-getSQL('Intranet',query=query1)

query2<-"
DECLARE @Date1 SMALLDATETIME
DECLARE @Date2 SMALLDATETIME

SET @Date1 = (SELECT Start_Date FROM Intranet..Global_Weeks WHERE DateDiff(d,365,GetDate()) BETWEEN Start_Date AND End_Date)
SET @Date2 = (SELECT End_Date FROM Intranet..Global_Weeks WHERE DateDiff(d,7,GetDate()) BETWEEN Start_Date AND End_Date)
    
SELECT DISTINCT B.StudID, 
     CASE
      WHEN Destination='EC' THEN 'ElkCreek' 
      WHEN Destination='Y5' THEN 'PraireView' 
      WHEN Destination='PT' THEN 'PlumThicket'
      ELSE NULL
      END AS Dest,
     A.Batch,
     CASE
     WHEN DATEPART(dw,DateShipped) = 1 THEN '7-Sunday'
     WHEN DATEPART(dw,DateShipped) = 2 THEN '1-Monday'
     WHEN DATEPART(dw,DateShipped) = 3 THEN '2-Tuesday'
     WHEN DATEPART(dw,DateShipped) = 4 THEN '3-Wednesday'
     WHEN DATEPART(dw,DateShipped) = 5 THEN '4-Thursday'
     WHEN DATEPART(dw,DateShipped) = 6 THEN '5-Friday'
     WHEN DATEPART(dw,DateShipped) = 7 THEN '6-Saturday'
     ELSE NULL
     END AS [Day],
     B.SPGid AS Name,
     B.BoarID,
     B.Breed,
     B.Doses,
     --E.[Target] AS CurTarget,
     --E2.[Target] AS PrevWkTarget,
     DateAdd(d,1,DateShipped) AS Day1,
     DateAdd(d,5,DateShipped) AS Day6,
     Idx,
     maxDate,
     G.SireID
     
FROM Geneticists.Bender.ExternalSemenDNA B
           INNER JOIN Intranet..Global_Weeks A ON B.DateShipped BETWEEN A.Start_Date AND A.End_Date
           LEFT OUTER JOIN BlupDB.Final.Pedigree G ON B.SPGid=G.SPGid
           LEFT OUTER JOIN 
            (SELECT A.spg_id as SPGID, idx, maxDate
              FROM Geneticists.Bender.idxHistory A
                    INNER JOIN (SELECT spg_id, MAX(idx_date) AS maxDate
                        FROM Geneticists.Bender.idxHistory
                        GROUP BY spg_id) C ON A.spg_id=C.spg_id AND A.idx_date=C.maxDate
            ) AS F ON B.SPGid=F.SPGid
     WHERE DateShipped >= '11/30/2020'
           AND DateShipped between @Date1 and @Date2
           AND B.Breed LIKE 'DNA%'
           
UNION
    
SELECT DISTINCT B.StudID, 
     CASE
      WHEN Destination='EC' THEN 'ElkCreek' 
      WHEN Destination='Y5' THEN 'PraireView' 
      WHEN Destination='PT' THEN 'PlumThicket'
      ELSE NULL
      END AS Dest,
     A.Batch,
     CASE
     WHEN DATEPART(dw,DateShipped) = 1 THEN '7-Sunday'
     WHEN DATEPART(dw,DateShipped) = 2 THEN '1-Monday'
     WHEN DATEPART(dw,DateShipped) = 3 THEN '2-Tuesday'
     WHEN DATEPART(dw,DateShipped) = 4 THEN '3-Wednesday'
     WHEN DATEPART(dw,DateShipped) = 5 THEN '4-Thursday'
     WHEN DATEPART(dw,DateShipped) = 6 THEN '5-Friday'
     WHEN DATEPART(dw,DateShipped) = 7 THEN '6-Saturday'
     ELSE NULL
     END AS [Day],
     B.SPGid AS Name,
     B.BoarID,
     B.Breed,
     B.Doses,
     --E.[Target] AS CurTarget,
     --E2.[Target] AS PrevWkTarget,
     DateAdd(d,1,DateShipped) AS Day1,
     DateAdd(d,5,DateShipped) AS Day6,
     Idx,
     maxDate,
     G.SireID
     
FROM Geneticists.Bender.ExternalSemenTN B
           INNER JOIN Intranet..Global_Weeks A ON B.DateShipped BETWEEN A.Start_Date AND A.End_Date
           LEFT OUTER JOIN BlupDB.Final.Pedigree G ON B.SPGid=G.SPGid
           LEFT OUTER JOIN 
            (SELECT A.spg_id as SPGID, idx, maxDate
              FROM Geneticists.Bender.idxHistory A
                    INNER JOIN (SELECT spg_id, MAX(idx_date) AS maxDate
                        FROM Geneticists.Bender.idxHistory
                        GROUP BY spg_id) C ON A.spg_id=C.spg_id AND A.idx_date=C.maxDate
            ) AS F ON B.SPGid=F.SPGid
     WHERE DateShipped >= '11/30/2020'
           AND DateShipped between @Date1 and @Date2
           AND B.Breed LIKE 'TZZZZ%'"

ext2<-getSQL('Intranet',query=query2)