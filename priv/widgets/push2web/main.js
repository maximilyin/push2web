(function() {
    var widget_id = getId(), apns_name = 'web.im.hypertalk', http_queue = [], 
        hostname = location.hostname, xhr = new XMLHttpRequest();

    /****** Load manifest ***********/
    var manifest = document.createElement('link');
    manifest.href = 'widgets/push2web/manifest.json';
    manifest.setAttribute('rel', 'manifest');
    document.head.appendChild(manifest);

    http_queue.push('auth_widget?widget_id=' + widget_id + '&hostname=' + hostname);
    http_req();

    function subscribe_notifications() {
        if(getNameBrowser() == 'Chrome' || 'Firefox' && 'serviceWorker' in navigator) {
            // Register ServiceWorker
            navigator.serviceWorker.register('service-worker.js')
            .then(function(e) {
                if(navigator.serviceWorker.controller) { 
                    console.log('ServiceWorker registration successful');
                    send_gcm_token();
                } else { location.reload() }
            }).catch(function(err) {
                console.log('ServiceWorker registration failed: ', err);
            })
        } else if (getNameBrowser() == 'Safari' && 'safari' in window) { 
            var obj = window.safari.pushNotification.permission(apns_name);
            send_apns_token(obj);
        } else { 
            console.log("Your device don't support push notifications"); 
        }
    };

    function send_apns_token(obj) {
        if(obj.permission == 'default') {
            window.safari.pushNotification.requestPermission(
                location.protocol+'//'+service_url, apns_name, {}, check_permission_apns
            );
        } else if(obj.permission == 'denied') {
            console.log('Push notification for Safari access dinied: ', obj);
        } else if (obj.permission == 'granted') {
            console.log('Subscription true');
            //request({action:'subscription.register', args:[obj.deviceToken, 'apns']});
        }
    };

    //Subscribe push notification
    function send_gcm_token() {
        navigator.serviceWorker.ready.then(function(SWRegistration) {
            SWRegistration.pushManager.subscribe({userVisibleOnly: true}).then(function(s) {
                if(Notification.permission === 'granted') {
                    var token = s.subscriptionId && s.endpoint.indexOf(s.subscriptionId) === -1 ?
                                s.endpoint + '/' + s.subscriptionId : s.endpoint;
                        var splitedUrl = token.split("/"),
                            id = splitedUrl[splitedUrl.length-1];
                        http_queue.push('subscription_id/' + id);
                        http_queue.push('subscription?device_token='+id+'&type=gcm&'+'widget_id='+widget_id);
                        http_req();
                        
                } else if (Notification.permission === 'denied') {
                    console.warn('Permission for Notifications was denied');
                    Notification.requestPermission();
                } else { console.error('Unable to subscribe to push', subscription); }
            }).catch(function(e) { 
                console.log('Subscribe push wrong:', e); 
            })
        })
    };

    function http_req() {
        if (xhr.readyState == 0 || xhr.readyState == 4 && http_queue.length != 0) {
            xhr.open('GET', http_queue[0], true);
            xhr.send();
            http_queue.splice(0, 1);
            http_req();
        } else if (http_queue.length != 0) { 
            setTimeout(function() { http_req() }, 200); 
        } else {}

        xhr.onloadend = function() {
            http_callback(xhr.status, xhr.responseText, xhr.responseURL);
        }
    };

    function http_callback(status, data, url) {
        if (status == 200 && data == 'auth_widget=ok') {
            subscribe_notifications();
        }
    };

    function getId(){
        var id = null, scripts, match;
        scripts = document.getElementsByTagName('script');
        for (var i = 0; i < scripts.length; i++) {
            match = scripts[i].src.match(/id=([-\a-zA-Z0-9]+)/);
            if(match){ id = match[1] }
        };
        
        return id
    };

    function getNameBrowser() {
        var ua = navigator.userAgent;    
        if (ua.search(/Chrome/) > 0) return 'Chrome';
        if (ua.search(/Firefox/) > 0) return 'Firefox';
        if (ua.search(/Opera/) > 0) return 'Opera';
        if (ua.search(/Safari/) > 0) return 'Safari';
        if (ua.search(/MSIE/) > 0) return 'IE';
    };
})();
