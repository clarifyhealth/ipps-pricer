-- use netmap_central;
-- use netmap_remote;

create table sync_control (
	last_modify_date int(25) not null
);

create table sync_tables (
	table_name	varchar(64) not null primary key,
	inbound char(1) not null check (value in ('Y', 'N')),
	outbound char(1) not null check (value in ('Y', 'N')),
	in_order int not null default 0
);