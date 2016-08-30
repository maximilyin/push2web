-module(push2web_db).
-export([query/2]).

query(get_widget_customer, Args) ->
    Query = "SELECT id, hostname FROM widgets_customers WHERE id = $1",
    execute(Query, Args);
query(get_customer, Args) ->
    Query = "SELECT id, password FROM customers WHERE id = $1",
    execute(Query, Args);
query(get_target_url, Args) ->
    Query = "SELECT id, hostname FROM widgets_customers WHERE customer_id = $1 AND hostname = $2",
    execute(Query, Args);
query(get_message, Args) ->
    Query = "SELECT id, widget_customer_id FROM messages WHERE status = 0",
    execute(Query, Args);
query(get_device_token, Args) ->
    Query = "SELECT device_token FROM subscriptions WHERE widget_customer_id = $1",
    execute(Query, Args);
query(get_wc_id, Args) ->
    Query = "SELECT widget_customer_id FROM subscriptions WHERE device_token = $1 and status = 1",
    execute(Query, Args);
query(get_last_message, Args) ->
    Query = "SELECT title, icon, body from messages WHERE widget_customer_id = $1 and status = 1 order by datetime_created desc limit 1",
    execute(Query, Args);
query(update_message_status, Args) ->
    Query = "UPDATE messages SET status = $1 WHERE id = $2",
    execute(Query, Args);
query(put_subscription, Args) ->
    Query = "INSERT INTO subscriptions (widget_customer_id, device_token, hostname, type, status) VALUES ($1, $2, $3, $4, $5)",
    execute(Query, Args);
query(put_message, Args) ->
    Query = "INSERT INTO messages (id, widget_customer_id, title, hostname, icon, body, datetime_expired) VALUES ($1, $2, $3, $4, $5, $6, $7)",
    execute(Query, Args).

execute(Query, Args) ->
    Fun = fun(Worker) ->
        epgsql:equery(Worker, Query, Args)
    end,
    octopus:perform(query, Fun).
