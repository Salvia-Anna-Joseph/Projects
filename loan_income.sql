-- create database income_loan;
-- use income_loan;
-- create table income_loan;
use income_loan;
select region,
         CASE 
           when (region='Yorks&Humber' or region='E Anglia') then 'south'
           when (region='E midlands'or region='W midlands') then 'midlands'
           when (region='North West'or region='South West') then 'west'
           when (region='South East'or region='South West') then 'south'
           end as new_region
from loan;
select sex, region, earner, avg(income_average), avg(loan_average) from loan GROUP BY sex,earner,region
order by sex;
