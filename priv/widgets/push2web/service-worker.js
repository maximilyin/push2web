'use strict';

var path = '/message_log.last?id=';

self.addEventListener('install', function(event) {
  console.log("ServiceWorker installed");
});

self.addEventListener('activate', function(event) {
  console.log("ServiceWorker activated");
});

self.addEventListener('push', function(event, e) {
    event.waitUntil(
        caches.open('cache').then(function(cache) {
            return cache.match(new Request('cache')).then(function (cacheResponse) {
                return cacheResponse.json().then(function(obj){
                    var id = obj.subscription_id;
                    return fetch(path + id, {credentials:'same-origin'}).then(function(response) {
                        if (response.status !== 200) {
                            console.log('Error get message: ' + response.status);
                            throw new Error();
                        }

                        return response.json().then(function(msg) {
                            if (msg.status == 'error') {
                                console.log('Error get message: ', msg.description)
                                throw new Error();
                            } else if (msg.args.body != '') {
                                return self.registration.showNotification(msg.args.title, {
                                    body: msg.args.body,
                                    icon: msg.args.icon,
                                    tag: 'Info',
                                    vibrate: [300, 100, 400]
                                })
                            }
                        });

                    }).catch(function(error) {
                        console.log('Unable to retrieve data: ', error);

                        return self.registration.showNotification('Push2Web', {
                            body: 'Ви маєте нове повідомлення...',
                            icon: 'widgets/push2web/img/logo.png',
                            tag: 'Info',
                            vibrate: [300, 100, 400]
                        })
                    })
                }).catch(function(e) { console.log('Error match json') })
            })
        })
    )
});


self.addEventListener('notificationclick', function(event) {
    event.notification.close();

    event.waitUntil(
        clients.matchAll({
            type: "window"
        })
            .then(function(clientList) {
                for (var i = 0; i < clientList.length; i++) {
                    var client = clientList[i];
                    if (client.url == '/' && 'focus' in client)
                        return client.focus();
                }
                if (clients.openWindow) {
                    return clients.openWindow('/');
                }
            })
    );
});

self.addEventListener('fetch', function(event) {
    var splitedUrl = event.request.url.split("/"),
        operation = splitedUrl[splitedUrl.length-2],
        id = splitedUrl[splitedUrl.length-1];
    if(operation == "subscription_id"){
        event.waitUntil(
            caches.open('cache').then(function(cache) {
                return cache.match(new Request('cache')).then(function (cacheResponse) {
                    return cacheResponse.json().then(function(obj){
                        obj.subscription_id = id;
                        var json = JSON.stringify(obj);
                        return cache.put(new Request('cache'), new Response(json));
                    }).catch(function(error){
                        var json = JSON.stringify({'subscription_id': id});
                        return cache.put(new Request('cache'), new Response(json));
                    })
                }).catch(function(e) {
                    var json = JSON.stringify({'subscription_id': id});
                    return cache.put(new Request('cache'), new Response(json));

                })
            })
        );

        event.respondWith(new Response());
    } else {
        event.respondWith(fetch(event.request).then(
            function(response) { return response; }));
    }
});
