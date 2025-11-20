CREATE DATABASE Bilbasen;
USE Bilbasen;

-- Invariant: dealer
CREATE TABLE dealer (
    dealer_id      VARCHAR(255) PRIMARY KEY,
    seller_name    VARCHAR(255),
    seller_address VARCHAR(255),
    seller_cvr     VARCHAR(20)
);

-- Invariant: car
CREATE TABLE car (
    carid       VARCHAR(255) PRIMARY KEY,
    makemodel   VARCHAR(255),
    props       VARCHAR(255),
    link        VARCHAR(500),
    dealer_id   VARCHAR(10),
    FOREIGN KEY (dealer_id) REFERENCES dealer(dealer_id)
);


-- Tidsserie: observationer på bil
-- ==============================
CREATE TABLE car_observation (
    obs_id     INT AUTO_INCREMENT PRIMARY KEY,
    carid      VARCHAR(255),
    price      DECIMAL(10,2),
    scrapedate DATE,
    sold BOOLEAN NOT NULL DEFAULT FALSE,
    FOREIGN KEY (carid) REFERENCES car(carid)
);

ALTER TABLE car_observation
ADD COLUMN sold TINYINT(1) NOT NULL DEFAULT 0;
