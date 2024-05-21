--ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN distance double precision NOT NULL;


CREATE TABLE atlas_driver_offer_bpp.rider_details (
id character(36) NOT NULL,
mobile_country_code character varying(255) NOT NULL,
mobile_number_encrypted character varying(255) NOT NULL,
mobile_number_hash bytea,
created_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL,
updated_at timestamp with time zone DEFAULT CURRENT_TIMESTAMP NOT NULL
,CONSTRAINT  ride_details_unique_mobile_number UNIQUE (mobile_number_hash, mobile_country_code)
,CONSTRAINT  rider_details_pkey PRIMARY KEY (id)
);
ALTER TABLE atlas_driver_offer_bpp.rider_details OWNER TO atlas_driver_offer_bpp_user;

-- ride
ALTER TABLE atlas_driver_offer_bpp.ride
  ALTER COLUMN fare SET DATA TYPE integer
  USING round(fare);

ALTER TABLE atlas_driver_offer_bpp.ride ADD CONSTRAINT fk_booking_id FOREIGN KEY (booking_id) REFERENCES atlas_driver_offer_bpp.booking(id);
ALTER TABLE atlas_driver_offer_bpp.ride OWNER TO atlas_driver_offer_bpp_user;
