<!DOCTYPE html>
<html lang="ru" prefix="og: http://ogp.me/ns# fb: http://ogp.me/ns/fb# article: http://ogp.me/ns/article#">
<apply template="head"/>

<body itemscope="itemscope" itemtype="http://schema.org/BlogPosting" class="${mobile}">
<apply template="author"/>
<apply template="topnav"/>

<div class="container">
    <div class="component-panel">
        <post/>
        <div id="disqus_thread"><comments/></div>
        <disqusVars />
        <script type="text/javascript">
            (function () {
                if (navigator.userAgent.indexOf('Opera Mini') === -1) {
                    var dsq = document.createElement('script');
                    dsq.type = 'text/javascript';
                    dsq.async = true;
                    dsq.src = 'http://' + disqus_shortname + '.disqus.com/embed.js';
                    (document.getElementsByTagName('head')[0] || document.getElementsByTagName('body')[0]).appendChild(dsq);
                }
            })();
        </script>
        <noscript>Please enable JavaScript to view the <a
                href="http://disqus.com/?ref_noscript">comments powered by
            Disqus.</a></noscript>
        <a href="http://disqus.com" class="dsq-brlink">comments powered by <span
                class="logo-disqus">Disqus</span></a>
    </div>
    <apply template="footer"/>

</div>
<!-- /container -->

<apply template="foot"/>

</body>
</html>
