ALTER TABLE atlas_app.person ADD COLUMN notification_token character varying(255);
ALTER TABLE atlas_app.merchant_service_usage_config
    ADD COLUMN notify_person character varying(30);
UPDATE atlas_app.merchant_service_usage_config
    SET notify_person = 'FCM'; -- FCM | PayTM
ALTER TABLE atlas_app.merchant_service_usage_config ALTER COLUMN notify_person SET NOT NULL;

WITH MerchantNotificationServiceConfigs AS (
  SELECT T1.id, 'Notification_FCM', CAST ('{
   "fcmUrl":"https://fcm.googleapis.com/v1/projects/jp-beckn-dev/messages:send/",
   "fcmServiceAccount":"xxxxxxxx",
   "fcmTokenKeyPrefix":"NAMMA_YATRI"
  }' AS json)
  FROM atlas_app.merchant AS T1
)
INSERT INTO atlas_app.merchant_service_config (merchant_id, service_name, config_json)
  (SELECT * FROM MerchantNotificationServiceConfigs);

-------------------------------------------------------------------------------------------
-------------------------------DROPS-------------------------------------------------------
-------------------------------------------------------------------------------------------
ALTER TABLE atlas_app.merchant DROP COLUMN fcm_url;
ALTER TABLE atlas_app.merchant DROP COLUMN fcm_service_account;
ALTER TABLE atlas_app.merchant DROP COLUMN fcm_redis_token_key_prefix;