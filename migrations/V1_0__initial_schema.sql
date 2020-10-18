create table if not exists todo (
  id uuid primary key,
  description varchar(1024) not null,
  completed bool
);
