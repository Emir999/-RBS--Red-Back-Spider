create table blocks (
    height integer primary key,
    block_id signature_type,
    block_timestamp timestamp not null,
    generator_address address_type,
    block_data_bytes blob not null
);

create table waves_balances (
    address address_type,
    regular_balance amount_type,
    effective_balance amount_type,
    height integer references blocks(height) on delete cascade,
    primary key (address, height)
);

create index regular_balance_index on waves_balances(regular_balance);

create table asset_info (
    asset_id digest_type primary key,
    issuer public_key_type,
    decimals int2 not null,
    name blob not null,
    description blob not null,
    height integer references blocks(height) on delete cascade
);

create index asset_info_height_index on asset_info(height);

create table asset_quantity (
    asset_id digest_type references asset_info(asset_id) on delete cascade,
    total_quantity numeric not null,
    reissuable boolean not null,
    height integer references blocks(height) on delete cascade,
    primary key (asset_id, height)
);

create index asset_quantity_height_index on asset_quantity(height);

create table asset_balances (
    address address_type,
    asset_id digest_type references asset_info(asset_id) on delete cascade,
    balance amount_type,
    height integer references blocks(height) on delete cascade,
    primary key (address, asset_id, height)
);

create index asset_balances_height_index on asset_balances(height);

create table lease_info (
    lease_id digest_type primary key,
    sender public_key_type,
    recipient address_or_alias,
    amount amount_type,
    height integer references blocks(height) on delete cascade
);

create index lease_info_height_index on lease_info(height);

create table lease_status (
    lease_id digest_type references lease_info(lease_id) on delete cascade,
    active boolean not null,
    height integer references blocks(height) on delete cascade
);

create index lease_status_height_index on lease_status(height);
create index lease_status_lease_id_index on lease_status(lease_id);

create table lease_balances (
    address address_type,
    lease_in bigint not null,
    lease_out bigint not null,
    height integer references blocks(height) on delete cascade,

    constraint non_negative_lease_in check (height < 462000 or lease_in >= 0),
    constraint non_negative_lease_out check (height < 462000 or lease_out >= 0),

    primary key (address, height)
);

create index lease_balances_height_index on lease_balances(height);

create table filled_quantity (
    order_id digest_type,
    filled_quantity amount_type,
    fee amount_type,
    height integer references blocks(height) on delete cascade,

    primary key (order_id, height)
);

create index filled_quantity_height_index on filled_quantity(height);

create table transactions (
    tx_id digest_type,
    signature signature_type,
    tx_type tx_type_id_type not null,
    height integer references blocks(height) on delete cascade,

    primary key (tx_id, signature)
);

create index transactions_height_index on transactions(height);

create table address_transaction_ids (
    address address_type,
    tx_id digest_type,
    signature signature_type,
    height integer references blocks(height) on delete cascade,

    foreign key (tx_id, signature) references transactions(tx_id, signature) on delete cascade
);

create index address_transaction_ids_tx_id_signature_index on address_transaction_ids(tx_id, signature);
create index address_transaction_ids_height_index on address_transaction_ids(height);

create table payment_transactions (
    tx_hash digest_type primary key,
    height integer references blocks(height) on delete cascade
);

create index payment_transactions_height_index on payment_transactions(height);

create table aliases (
    alias blob primary key,
    address address_type,
    height integer references blocks(height) on delete cascade
);

create index aliases_of_address_index on aliases(address);
create index aliases_height_index on aliases(height);
