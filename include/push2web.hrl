%% LAGER MACROS
-define(LOG_DEBUG(Format, Args),    lager:debug(Format, Args)).
-define(LOG_INFO(Format, Args),     lager:info(Format, Args)).
-define(LOG_WARNING(Format, Args),  lager:warning(Format, Args)).
-define(LOG_ERROR(Format, Args),    lager:error(Format, Args)).
-define(LOG_CRITICAL(Format, Args), lager:critical(Format, Args)).

-define(DEFAULT_ICON, <<"https://hypertalk.im/widgets/push/img/logo.png">>).
-define(DEFAULT_TTL, 3600).
-define(DEFAULT_TITLE, <<"You got push notification!">>).
