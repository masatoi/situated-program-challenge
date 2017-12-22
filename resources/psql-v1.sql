create table meetups (
    id              serial primary key,
    title           text not null, 
    start_at        timestamp,
    end_at          timestamp,
    venue_id        int
);
--;;
create index meetups_title on meetups (title);
--;;
create table members (
    id               serial primary key,
    first_name       text,
    last_name        text,
    email            text
);
--;;
create index members_email on members (email);
--;;
create table venues (
    id               serial primary key,
    name             text,
    postal_code      text,
    prefecture       text,
    city             text,
    street1          text,
    street2          text,
    building         text
);
--;;
create index venues_name on venues (name);
--;;
create table meetups_members (
    meetup_id     int,
    member_id     int,
    primary key (meetup_id, member_id)
);

create table groups (
   id            serial primary key, 
   name          text,
   created_at    timestamp
);
--;;
create index groups_name on groups (name);
--;;
alter table meetups add column group_id int;
--;;
alter table venues add column group_id int;
--;;
create table groups_members (
    group_id    int,
    member_id   int,
    primary key (group_id, member_id)
);


alter table groups_members add column admin boolean default false;

alter table venues drop column building;

