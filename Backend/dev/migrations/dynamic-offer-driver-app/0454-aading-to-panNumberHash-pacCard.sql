CREATE INDEX idx_pan_card_number_hash ON atlas_driver_offer_bpp.driver_pan_card USING btree (pan_card_number_hash);
ALTER TABLE driver_pan_card DROP CONSTRAINT unique_pan_card;