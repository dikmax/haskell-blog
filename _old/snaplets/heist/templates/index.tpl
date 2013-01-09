<!DOCTYPE html>
<html lang="ru" prefix="og: http://ogp.me/ns# fb: http://ogp.me/ns/fb# article: http://ogp.me/ns/article#">
<apply template="head"/>

<body itemscope="itemscope" itemtype="http://schema.org/Blog" class="${mobile}">
<meta itemprop="name" content="[dikmax's blog]"/>
<apply template="author"/>
<apply template="topnav"/>

<div class="container">
    <posts/>
    <pagination/>
    <apply template="footer"/>
</div>
<!-- /container -->

<apply template="foot"/>
<disqusVars />
<script type="text/javascript">
    /* * * DON'T EDIT BELOW THIS LINE * * */
    (function () {
        var s = document.createElement('script');
        s.async = true;
        s.type = 'text/javascript';
        s.src = 'http://' + disqus_shortname + '.disqus.com/count.js';
        (document.getElementsByTagName('HEAD')[0] || document.getElementsByTagName('BODY')[0]).appendChild(s);
    }());
</script>
</body>
</html>
