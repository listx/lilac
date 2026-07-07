(function() {
    let reloading = false;
    const path = window.location.pathname;
    const wsUrl = (window.location.protocol === 'https:' ? 'wss://' : 'ws://')
                  + window.location.host
                  + '/lilac-ws';

    const scrollSelectors = ['nav.toc', 'main', 'aside.sidebar']; //
    const storageKey = 'dev-scroll-map';

    let lastScrollTime = 0;
    window.addEventListener('scroll', () => { //
      lastScrollTime = Date.now();
    }, true);

    function setScrollPositions(map) {
      for (let sel in map) {
        let el = document.querySelector(sel);
        if (el) el.scrollTop = map[sel];
      }
    }

    function getScrollPositions(selectors) {
      let map = {};
      selectors.forEach(sel => {
        let el = document.querySelector(sel);
        if (el) map[sel] = el.scrollTop;
      });
      return map;
    }

    const rawData = localStorage.getItem(storageKey);
    if (rawData) {
      const prevScrollPositions = JSON.parse(rawData);
      setTimeout(() => {
        setScrollPositions(prevScrollPositions); //
        localStorage.removeItem(storageKey);
      }, 200); //
    }

    //
    function reloadPage() {
      const recentlyScrolled = (Date.now() - lastScrollTime) < 1500; //

      if (recentlyScrolled) { //
        setTimeout(reloadPage, 500); //
        return;
      }

      const currentScrollPositions = getScrollPositions(scrollSelectors);
      localStorage.setItem(storageKey,
                           JSON.stringify(currentScrollPositions)); //

      window.location.reload(); //
    }

    function connectWebSocket() {
        const ws = new WebSocket(wsUrl); //

        const trigger = () => {
            if (!reloading) {
                reloading = true;
                console.log("Lilac: Initiating reload...");
                reloadPage();
            }
        };

        ws.onopen = () => {
            console.log("Lilac: WebSocket connected. Monitoring path:", path);
            ws.send(path); //

            //
            const heartbeat = setInterval(() => {
                if (ws.readyState === WebSocket.OPEN) {
                    ws.send('LILAC_WS_HEARTBEAT_FROM_JS_CLIENT');
                } else {
                    clearInterval(heartbeat);
                }
            }, 30000);
        };

        ws.onmessage = (msg) => {
            if (msg.data === 'reload') {
                console.log("Lilac: Reload signal received."); //
                trigger();
            }
        };

        ws.onclose = (e) => {
            if (!reloading) {
                console.log("Lilac: WebSocket closed (Code " + e.code + ").");
                console.log("Lilac: Reconnecting in 2s...");
                setTimeout(connectWebSocket, 2000); //
            }
        };

        ws.onerror = (err) => {
            console.error("Lilac: WebSocket error observed:", err);
            ws.close(); //
        };
    }

    connectWebSocket();
})();
