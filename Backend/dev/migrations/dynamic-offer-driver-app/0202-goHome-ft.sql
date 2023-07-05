CREATE TABLE atlas_driver_offer_bpp.driver_home_location(
    id character(36) NOT NULL PRIMARY KEY,
    driver_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.person (id),
    lat float NOT NULL,
    lon float NOT NULL,
    created_at timestamp with time zone NOT NULL
);

CREATE TABLE atlas_driver_offer_bpp.driver_go_home_request(
    id character(36) NOT NULL PRIMARY KEY,
    driver_id character(36) NOT NULL REFERENCES atlas_driver_offer_bpp.person (id),
    lat float NOT NULL,
    lon float NOT NULL,
    point public.geography(Point,4326) NOT NULL,
    status character varying(36) NOT NULL,
    created_at timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
);

ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN go_home_from_location_radius integer;
UPDATE atlas_driver_offer_bpp.driver_pool_config AS T1 SET go_home_from_location_radius = max_radius_of_search;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ALTER COLUMN go_home_from_location_radius SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ADD COLUMN go_home_to_location_radius integer;
UPDATE atlas_driver_offer_bpp.driver_pool_config AS T1 SET go_home_to_location_radius = max_radius_of_search;
ALTER TABLE atlas_driver_offer_bpp.driver_pool_config ALTER COLUMN go_home_to_location_radius SET NOT NULL;

ALTER TABLE atlas_driver_offer_bpp.search_request_for_driver ADD COLUMN go_home_request_id character(36) REFERENCES atlas_driver_offer_bpp.driver_go_home_request (id);
ALTER TABLE atlas_driver_offer_bpp.booking ADD COLUMN go_home_request_id character(36) REFERENCES atlas_driver_offer_bpp.driver_go_home_request (id);
ALTER TABLE atlas_driver_offer_bpp.driver_quote ADD COLUMN go_home_request_id character(36) REFERENCES atlas_driver_offer_bpp.driver_go_home_request (id);
