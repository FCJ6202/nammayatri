CREATE TABLE IF NOT EXISTS atlas_driver_offer_bpp._vehicle_registration_cert_t    
(
    id character(36) COLLATE pg_catalog."default" NOT NULL,
    driver_id character varying(255) COLLATE pg_catalog."default" NOT NULL,
    vehicle_registration_cert_number character varying(255) COLLATE pg_catalog."default",
    fitness_cert_expiry  timestamp with time zone,
    permit_number character varying(255) COLLATE pg_catalog."default",
    permit_start timestamp with time zone,
    permit_expiry timestamp with time zone,
    vehicle_class character(36) COLLATE pg_catalog."default",
    vehicle_number character(36) COLLATE pg_catalog."default",
    insurance_validity timestamp with time zone,
    request_id character(36) COLLATE pg_catalog."default" NOT NULL,
    created_at timestamp with time zone NOT NULL,
    idfy_status character varying(20) NOT NULL,
    verification_status character varying(20) NOT NULL,
    consent boolean DEFAULT true NOT NULL,
    consent_timestamp timestamp with time zone NOT NULL,
    updated_at timestamp with time zone NOT NULL
    ,CONSTRAINT  VehicleRegistrationCert_driver_id_fkey FOREIGN KEY (driver_id) REFERENCES atlas_driver_offer_bpp.person(id)
);
