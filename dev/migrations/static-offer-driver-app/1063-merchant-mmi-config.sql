INSERT INTO atlas_transporter.merchant_service_config (merchant_id, service_name, config_json)
SELECT m.id, 'Maps_MMI',
  json_build_object(
     'mmiAuthUrl','https://outpost.mappls.com/api/security/oauth/token'
    , 'mmiAuthId','mmi-auth-id'
    , 'mmiAuthSecret','0.1.0|2|xSB5xFVBLFXGTlAZ1+82Lv5vACF4W4+W/G2ut066I3BA3VymnvTwAGMcJJcNGbjN3CPMEogiBSSg961uFw=='
    , 'mmiApiKey','0.1.0|2|xSB5xFVBLFXGTlAZ1+82Lv5vACF4W4+W/G2ut066I3BA3VymnvTwAGMcJJcNGbjN3CPMEogiBSSg961uFw=='
    , 'mmiKeyUrl','https://apis.mapmyindia.com/'
    , 'mmiNonKeyUrl','https://atlas.mapmyindia.com/'
  )
FROM atlas_transporter.merchant m;