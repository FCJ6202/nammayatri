CREATE TABLE atlas_driver_offer_bpp.registry_map_fallback (
  subscriber_id character(36) NOT NULL,
  unique_id character(36) NOT NULL,
  registry_url character varying(255) NOT NULL,
  PRIMARY KEY (subscriber_id, unique_id)
);
