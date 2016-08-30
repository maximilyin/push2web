CREATE TABLE widgets (
    id              	serial	    NOT NULL PRIMARY KEY,
    name	    	    varchar(8)  NOT NULL,
    description     	varchar(64) DEFAULT NULL,
    datetime_created    timestamp   NOT NULL DEFAULT current_timestamp
);

CREATE TABLE customers (
    id              	serial	    NOT NULL PRIMARY KEY,
    login	    	    varchar(32) NOT NULL,
    password	    	varchar(32) NOT NULL,
    description     	varchar(64) DEFAULT NULL,
    datetime_created    timestamp   NOT NULL DEFAULT current_timestamp
);

CREATE TABLE widgets_customers (
    id              	varchar(32) NOT NULL PRIMARY KEY,
    widget_id	    	integer     NOT NULL,
    customer_id     	integer     NOT NULL,
    hostname            varchar(32) NOT NULL,
    datetime_created    timestamp   NOT NULL DEFAULT current_timestamp,
    description     	varchar(64) DEFAULT NULL,
    status              smallint    DEFAULT 1
);

CREATE TABLE subscriptions (
    device_token        varchar(260)NOT NULL PRIMARY KEY,
    widget_customer_id  varchar(32) NOT NULL,
    hostname            varchar(32) NOT NULL,
    type                varchar(8)  NOT NULL,
    datetime_created    timestamp   NOT NULL DEFAULT current_timestamp,
    status              smallint    NOT NULL
);

CREATE TABLE messages (
    id                  varchar(32) NOT NULL PRIMARY KEY,
    widget_customer_id  varchar(32) NOT NULL,
    title               varchar(256)NOT NULL,
    hostname            varchar(32) NOT NULL,
    icon                varchar(128)NOT NULL,
    body                text        NOT NULL,
    datetime_created    timestamp   NOT NULL DEFAULT current_timestamp,
    datetime_sent       timestamp   NULL,
    datetime_delivered  timestamp   NULL,
    datetime_expired    timestamp   NULL,
    status              smallint    DEFAULT 0
);
