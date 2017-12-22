drop table meetups;
--;;
drop table members;
--;;
drop table venues;
--;;
drop table meetups_members;
drop table groups_members;
--;;
alter table venues drop column group_id;
--;;
alter table meetups drop column group_id;
--;;
drop table groups;

alter table groups_members drop column admin;
alter table venues add column building text;

