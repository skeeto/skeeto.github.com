---
title: Some Galois Linear Feedback Shift Generators
layout: post
date: 2019-02-06T02:17:35Z
tags: []
uuid: aaba5b62-48f5-3105-eb3f-ec298b798fa4
---

<div id="lfsr256"></div>
<div id="lfsr4096"></div>
<canvas id="lfsr256" width="600" height="600"></canvas>
<canvas id="lfsr4096" width="600" height="600"></canvas>
<script src="/script/lfsr.js"></script>
<script>
(function() {
    LFSR.setup('#lfsr256',  LFSR.lfsr256,  16, 16, 10);
    LFSR.setup('#lfsr4096', LFSR.lfsr4096, 64, 64, 20);
}());
</script>
