create extension if not exists "uuid-ossp";

create table if not exists todo (
  id uuid primary key,
  description varchar(1024) not null,
  completed bool,
  created_at timestamptz not null,
  last_updated_at timestamptz not null
);

create index if not exists todo_last_updated_at_idx on todo (last_updated_at desc);

insert into todo values
  (uuid_generate_v4(), 'First', false, now(), now()),
  (uuid_generate_v4(), 'Second', false, now(), now())
;
