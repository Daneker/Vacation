create table users (
      id bigserial primary key,
      email text not null unique
);

create table vacation_requests (
      id bigserial primary key,
      user_id bigint not null references users(id),

      vacation_type text not null check (vacation_type in ('Yearly')),
      start_date date not null,
      end_date date not null,
      substitute text, -- замена на период отпуска

      status text not null check (status in ('New', 'Declined', 'Accepted', 'Sent', 'Vacation')),
      decline_reason varchar(1000)
);

-- default naming conventions https://gist.github.com/popravich/d6816ef1653329fb1745
create index vr_user_id_end_date_idx on vacation_requests(user_id, end_date);
create index vr_status_start_date_idx on vacation_requests(status, start_date);