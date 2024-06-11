CREATE TABLE atlas_app.payout_orders ();

ALTER TABLE atlas_app.payout_orders ADD COLUMN account_details_type text ;
ALTER TABLE atlas_app.payout_orders ADD COLUMN amount double precision NOT NULL;
ALTER TABLE atlas_app.payout_orders ADD COLUMN city text NOT NULL;
ALTER TABLE atlas_app.payout_orders ADD COLUMN created_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.payout_orders ADD COLUMN customer_email text NOT NULL;
ALTER TABLE atlas_app.payout_orders ADD COLUMN customer_id text NOT NULL;
ALTER TABLE atlas_app.payout_orders ADD COLUMN id character varying(36) NOT NULL;
ALTER TABLE atlas_app.payout_orders ADD COLUMN merchant_id text NOT NULL;
ALTER TABLE atlas_app.payout_orders ADD COLUMN mobile_no text NOT NULL;
ALTER TABLE atlas_app.payout_orders ADD COLUMN order_id text NOT NULL;
ALTER TABLE atlas_app.payout_orders ADD COLUMN status text NOT NULL;
ALTER TABLE atlas_app.payout_orders ADD COLUMN updated_at timestamp with time zone NOT NULL default CURRENT_TIMESTAMP;
ALTER TABLE atlas_app.payout_orders ADD COLUMN vpa text ;
ALTER TABLE atlas_app.payout_orders ADD PRIMARY KEY ( id, order_id);