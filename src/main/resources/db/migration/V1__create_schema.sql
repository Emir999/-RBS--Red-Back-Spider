create domain public_key_type as binary(32) not null;
create domain address_type as binary(64) not null;
create domain digest_type as binary(64) not null;
create domain signature_type as binary(64) not null;
create domain amount_type as bigint not null check (value >= 0);
create domain height_type as int not null check (value > 0);


create table blocks (
    height height_type primary key,
    block_id signature_type,
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
    asset_id digest_type primary key,
    issuer public_key_type,
    decimals tinyint not null,
    name binary not null,
    description binary not null,
    height height_type references blocks(height) on delete cascade,
);

create table asset_quantity (
    asset_id digest_type references asset_info(asset_id),
    quantity amount_type,
    reissuable boolean not null,
    height height_type references blocks(height) on delete cascade,
    primary key (asset_id, height)
);

create table asset_balances (
    address public_key_type,
    asset_id digest_type references asset_info(asset_id),
    balance amount_type,
    height height_type references blocks(height) on delete cascade,
    primary key (address, asset_id, height)
);

create table lease_info (
    lease_id digest_type primary key,
    sender public_key_type,
    recipient public_key_type,
    amount amount_type,
    height height_type references blocks(height) on delete cascade
);

create table lease_status (
    lease_id digest_type references lease_info(lease_id) on delete cascade,
    tx_id digest_type not null,
    active boolean not null,
    height height_type references blocks(height) on delete cascade
);

create table filled_quantity (
    order_id digest_type,
    filled_quantity amount_type,
    height height_type references blocks(height) on delete cascade
);

create table transaction_offsets (
    tx_id digest_type primary key,
    start_offset int not null,
    height height_type references blocks(height) on delete cascade
);

create table address_transaction_ids (
    address address_type,
    tx_id digest_type references transaction_offsets(tx_id) on delete cascade,
    height height_type references blocks(height) on delete cascade,

    primary key (address, tx_id, height)
);

create table payment_transactions (
    tx_hash digest_type primary key,
    height height_type references blocks(height) on delete cascade
);
