create domain address_type as binary(64);
create domain asset_id_type as binary(64);
create domain tx_id_type as binary(64);
create domain order_id_type as binary(64);

create table waves_balances (
    address address_type not null,
    balance bigint not null,
    height int not null,
    primary key (address, height)
);

create table asset_info (
    asset_id asset_id_type primary key,
    decimals tinyint not null,
    name text not null,
    description text not null,
    reissuable boolean not null default false
);

create table asset_balances (
    address address_type not null,
    asset_id asset_id_type not null references asset_info(asset_id),
    balance bigint not null,
    height int not null,
    primary key (address, asset_id, height)
);

create table asset_quantity (
    asset_id asset_id_type not null references asset_info(asset_id),
    quantity bigint not null,
    height int not null,
    primary key (asset_id, height)
);

create table active_leases (
    lease_id tx_id_type primary key,
    source_address address_type not null,
    target_address address_type not null,
    amount bigint not null,
    height int not null
);

create table remaining_fill_quantity (
    order_id order_id_type not null,
    remaining_quantity bigint not null,
    height int not null
);