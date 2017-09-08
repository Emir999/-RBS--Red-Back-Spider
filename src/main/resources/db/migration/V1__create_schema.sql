create domain address_type as binary(64) not null;
create domain asset_id_type as binary(64) not null;
create domain tx_id_type as binary(64) not null;
create domain order_id_type as binary(64) not null;
create domain block_id_type as binary(64) not null;
create domain amount_type as bigint not null check (value >= 0);
create domain height_type as int not null check (value > 0);


create table blocks (
    height height_type primary key,
    block_id block_id_type,
    block_data_bytes binary
);

create table waves_balances (
    address address_type,
    balance amount_type,
    lease_in amount_type,
    lease_out amount_type,
    height height_type references blocks(height) on delete cascade,
    primary key (address, height)
);

create table asset_info (
    asset_id asset_id_type primary key,
    decimals tinyint not null,
    name binary not null,
    description binary not null,
    height height_type references blocks(height) on delete cascade,
);

create table asset_quantity (
    asset_id asset_id_type references asset_info(asset_id),
    quantity amount_type,
    reissuable boolean not null,
    height height_type references blocks(height) on delete cascade,
    primary key (asset_id, height)
);

create table asset_balances (
    address address_type,
    asset_id asset_id_type references asset_info(asset_id),
    balance amount_type,
    height height_type references blocks(height) on delete cascade,
    primary key (address, asset_id, height)
);

create table active_leases (
    lease_id tx_id_type primary key,
    source_address address_type,
    target_address address_type,
    amount amount_type,
    height height_type references blocks(height) on delete cascade
);

create table filled_quantity (
    order_id order_id_type,
    filled_quantity amount_type,
    height height_type references blocks(height) on delete cascade
);

create table transaction_offsets (
    tx_id tx_id_type primary key,
    start_offset int not null,
    height height_type references blocks(height) on delete cascade
);

create table address_transaction_ids (
    address address_type,
    tx_id tx_id_type references transaction_offsets(tx_id) on delete cascade,
    height height_type references blocks(height) on delete cascade,

    primary key (address, tx_id, height)
);

create table payment_transactions (
    tx_hash tx_id_type primary key,
    height height_type references blocks(height) on delete cascade
);
