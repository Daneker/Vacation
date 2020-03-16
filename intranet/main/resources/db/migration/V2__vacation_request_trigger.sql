create function check_date_conflict()
returns trigger as
    $$
    begin
        perform *
        from vacation_requests
        where
            id <> new.id
            and user_id = new.user_id
            and (start_date between new.start_date and new.end_date
            or end_date between new.start_date and new.end_date
            or (start_date < new.start_date and end_date > new.end_date));
        if found then
            raise exception 'date conflict occurred';
        end if;
        return new;
    end;
    $$
    language plpgsql;


create trigger vacation_requests_ait
    after insert
    on vacation_requests
    for each row
    execute procedure check_date_conflict();

create function check_invalid_status_transition()
returns trigger as
    $$
    begin
        if not (
                (old.status = 'New' and (new.status = 'Accepted' or new.status = 'Declined'))
                or
                (old.status = 'Accepted' and new.status = 'Sent')
                or
                (old.status = 'Sent' and new.status = 'Vacation')
                or
                (old.status = new.status)
            ) then
            raise exception 'invalid status transition: you cannot change status from % to %', old.status, new.status;
        end if;
        return new;
    end;
    $$
    language plpgsql;

create trigger vacation_requests_aut
    after update
    of status
    on vacation_requests
    for each row
    execute procedure check_invalid_status_transition();

