$(function() {
    'use strict';

    hljs.initHighlighting();
    
    // Detect mobile
    var a = navigator.userAgent || navigator.vendor || window.opera;
    var isMobile = /android.+mobile|avantgo|bada\/|blackberry|blazer|compal|elaine|fennec|hiptop|iemobile|ip(hone|od)|iris|kindle|lge |maemo|midp|mmp|netfront|opera m(ob|in)i|palm( os)?|phone|p(ixi|re)\/|plucker|pocket|psp|symbian|treo|up\.(browser|link)|vodafone|wap|windows (ce|phone)|xda|xiino/i.test(a)
            || /1207|6310|6590|3gso|4thp|50[1-6]i|770s|802s|a wa|abac|ac(er|oo|s\-)|ai(ko|rn)|al(av|ca|co)|amoi|an(ex|ny|yw)|aptu|ar(ch|go)|as(te|us)|attw|au(di|\-m|r |s )|avan|be(ck|ll|nq)|bi(lb|rd)|bl(ac|az)|br(e|v)w|bumb|bw\-(n|u)|c55\/|capi|ccwa|cdm\-|cell|chtm|cldc|cmd\-|co(mp|nd)|craw|da(it|ll|ng)|dbte|dc\-s|devi|dica|dmob|do(c|p)o|ds(12|\-d)|el(49|ai)|em(l2|ul)|er(ic|k0)|esl8|ez([4-7]0|os|wa|ze)|fetc|fly(\-|_)|g1 u|g560|gene|gf\-5|g\-mo|go(\.w|od)|gr(ad|un)|haie|hcit|hd\-(m|p|t)|hei\-|hi(pt|ta)|hp( i|ip)|hs\-c|ht(c(\-| |_|a|g|p|s|t)|tp)|hu(aw|tc)|i\-(20|go|ma)|i230|iac( |\-|\/)|ibro|idea|ig01|ikom|im1k|inno|ipaq|iris|ja(t|v)a|jbro|jemu|jigs|kddi|keji|kgt( |\/)|klon|kpt |kwc\-|kyo(c|k)|le(no|xi)|lg( g|\/(k|l|u)|50|54|e\-|e\/|\-[a-w])|libw|lynx|m1\-w|m3ga|m50\/|ma(te|ui|xo)|mc(01|21|ca)|m\-cr|me(di|rc|ri)|mi(o8|oa|ts)|mmef|mo(01|02|bi|de|do|t(\-| |o|v)|zz)|mt(50|p1|v )|mwbp|mywa|n10[0-2]|n20[2-3]|n30(0|2)|n50(0|2|5)|n7(0(0|1)|10)|ne((c|m)\-|on|tf|wf|wg|wt)|nok(6|i)|nzph|o2im|op(ti|wv)|oran|owg1|p800|pan(a|d|t)|pdxg|pg(13|\-([1-8]|c))|phil|pire|pl(ay|uc)|pn\-2|po(ck|rt|se)|prox|psio|pt\-g|qa\-a|qc(07|12|21|32|60|\-[2-7]|i\-)|qtek|r380|r600|raks|rim9|ro(ve|zo)|s55\/|sa(ge|ma|mm|ms|ny|va)|sc(01|h\-|oo|p\-)|sdk\/|se(c(\-|0|1)|47|mc|nd|ri)|sgh\-|shar|sie(\-|m)|sk\-0|sl(45|id)|sm(al|ar|b3|it|t5)|so(ft|ny)|sp(01|h\-|v\-|v )|sy(01|mb)|t2(18|50)|t6(00|10|18)|ta(gt|lk)|tcl\-|tdg\-|tel(i|m)|tim\-|t\-mo|to(pl|sh)|ts(70|m\-|m3|m5)|tx\-9|up(\.b|g1|si)|utst|v400|v750|veri|vi(rg|te)|vk(40|5[0-3]|\-v)|vm40|voda|vulc|vx(52|53|60|61|70|80|81|83|85|98)|w3c(\-| )|webc|whit|wi(g |nc|nw)|wmlb|wonu|x700|xda(\-|2|g)|yas\-|your|zeto|zte\-/i.test(a.substr(0, 4));

    // Replace text in comments lint to proper Russian
    var commentsInterval = setInterval(function () {
        var counters = $('p.post-comments > a');
        if (counters.length === 0) {
            clearInterval(commentsInterval);
            return;
        }
        if ($(counters[0]).text() == 'Считаем комментарии...') {
            return;
        }
        clearInterval(commentsInterval);
        counters.each(function (i, item) {
            var $item = $(item);
            var text = $item.text();
            var match = text.match(/^(\d+) комментариев/);
            if (match) {
                var count = Number(match[1]);
                if (Math.round(count % 100 / 10) != 1) {
                    var count10 = count % 10;
                    if (count10 == 1) {
                        $item.text(count + ' комментарий');
                    } else if (count10 >= 2 && count10 <= 4) {
                        $item.text(count + ' комментария');
                    }
                }
            }            
        });
    }, 100);

    // Process code
    $('pre > code').each(function () {
        var $html = $(this).html();
        var lines = $html.split('\n');
        lines = $.map(lines, function (item, i) {
            return '<span class="line">' + item + '</span>';            
        });

        $(this).html(lines.join('\n'));
        $(this).addClass('highlighted');
    });

    // Inline footnotes
    $('.footnotes').each(function (i, footnotes) {
        var $footnotes = $(footnotes);
        $footnotes.find('li').each(function (i, item) {
            var $item = $(item);
            var forId = $item.attr('data-for');
            if (!forId) {
                return;
            }
            $("#" + forId).popover({
                placement: 'top',
                trigger: 'manual',
                title: "Примечание " + ($item.index() + 1),
                content: $item.html()
            });
            $('#' +forId).click(function (node) {
                $(this).popover('toggle');
            });
        });
        $footnotes.remove();
    });

    // Themes toggle
    var themesVisible = false;
    var themesSize = 0;
    $('.themes-box-toggle > a').click(function () {
        var smallScreen = $(window).width() < 980;
        if (!themesVisible) {
            if (smallScreen) {
                themesSize = $('.nav-collapse')[0].scrollHeight;
            }            
            $('.themes-box').show();            
            if (smallScreen) {
                var newH = $('.nav-collapse')[0].scrollHeight;
                $('.nav-collapse').height(newH);                
            }
            //$('.nav-collapse').collapse('show');
            themesVisible = true;
        } else {
            $('.themes-box').hide();
            if (smallScreen) {
                $('.nav-collapse').height(themesSize);
            }            
            themesVisible = false;            
        }
        return false;
    });
    $('body').on('click', function (e) {
        if (!themesVisible) {
            return;
        }
        var target = $(e.target);
        if (!target.hasClass('themes-box') && target.parents('.themes-box').length === 0
            || target.hasClass('themes-button')) {
            $('.themes-box').hide();
            var smallScreen = $(window).width() < 980;
            if (smallScreen) {
                $('.nav-collapse').height(themesSize);
            }            
            themesVisible = false;
        }
    });
    
    // Latest movies transform
    if ($('.latest-movies').length > 0) {
        var movies = [];
        $('.latest-movies li').each(function (i, item) {
            if (item.childNodes.length !== 3) {
                return;
            }
            var date = item.childNodes[0].data;
            var titleLink = $(item.childNodes[1]);
            var link = titleLink.attr('href');
            var title = titleLink.text();
            var image = titleLink.attr('title');
            var rating = item.childNodes[2].data.match(/\((\d+)\/10\)/);
            movies.push({
                date: date,
                link: link,
                rating: rating ? Number(rating[1]) : null,
                image: image,
                title: title
            });
        });
        var topSpan = $(document.createElement('span'));
        topSpan.addClass('wrap');
        $.each(movies, function (i, movie) {
            var item = $(document.createElement('span'));
            item.addClass('item');
            // Header
            var header = $(document.createElement('span'));
            header.addClass('item-header');
            header.text(movie.date);
            header.append($(document.createElement('br')));
            var link = $(document.createElement('a'));
            link.attr({
                'href': movie.link
            });
            link.text(movie.title);
            header.append(link);
            if (movie.rating) {
                header.append($(document.createElement('br')));
                header.append('<span class="rating" title="(' + movie.rating + '/10)"><span class="rate-' + movie.rating + '"></span></span>')
            }
            item.append(header);

            // Footer
            if (movie.image) {
                var footer = $(document.createElement('span'));
                footer.addClass('item-footer');
                var img = $(document.createElement('img'));
                img.attr({
                    'src': movie.image,
                    'title': movie.title
                });
                link = $(document.createElement('a'));
                link.attr({
                    'href': movie.link
                });
                link.append(img);
                footer.append(link);
                item.append(footer)
            }
            topSpan.append(item);            
        });
        $('.latest-movies ul').replaceWith(topSpan);
    }

    // Vault table list
    $('.vault-posts-list tbody tr').click(function() {
        document.location = '/vault/edit/' + $(this).attr('data-rowid');
    });

    $('.vault-posts-list span.action-delete').click(function() {
        var id = $(this).parents('tr').attr('data-rowid');
        if (confirm('Действительно удалить запись ' + id + '?')) {
            document.location = '/vault/delete/' + id;
        }
        return false;
    });

    $('.vault-posts-list span.action-view').click(function() {
        var url = $(this).parents('tr').attr('data-url');
        document.location = '/post/' + url;
        return false;
    });

    // Show mini statistics in vault
    if (document.location.pathname.indexOf('/vault') === 0) {        
        var publishedCount = 0;
        var posts = $('.vault-posts-list tr[data-rowid]');
        posts.each(function (i, item) {
            if ($(item).find('td:nth-child(2)').text() === '+') {
                ++publishedCount;
            }
        });
        $('.vault-posts-count').text('Записей: ' + posts.length);
        $('.vault-published-count').text('Опубликовано: ' + publishedCount)
    }

    // Refresh page in vault
    if (document.location.pathname.indexOf('/vault/edit') === 0) {
        $('#post-url').change(function () {
            var url = $(this).val();
            var id = $('#post-id').val();
            $.ajax('/vault/checkurl', {
                data: {
                    id: id,
                    url: url
                },
                dateType: 'json',
                type: 'POST',
                success: function (result) {
                    result = $.parseJSON(result);
                    $('#post-url').parents('.control-group').toggleClass('error', !result.result);
                }
            });
        });
        var refreshHandler = function () {
            $.post('/vault/renderpost', $('.post-form').serialize(),
                function (data) {
                    $('.render-area').html(data);
                },
                "html"
            );
            return false;
        };
        
        $('.btn-refresh').click(refreshHandler);
        refreshHandler()
        
        if (!isMobile) {
            var formData = $('.post-form').serialize();
            var updateTimeout;
            setInterval(function () {
                var newFormData = $('.post-form').serialize();
                if (formData != newFormData) {
                    if (updateTimeout) {
                        clearTimeout(updateTimeout);
                    }
                    formData = newFormData;
                    updateTimeout = setTimeout(function () {
                        updateTimeout = null;
                        refreshHandler();
                    }, 2000);
                }
            }, 1000);   
        }
    }
    
    // Vault files handlers
    var showError = function (object) {
        if (object.message) {
            alert("Произошла ошибка: " + object.message);
        } else {
            alert("Произошла неизвестная ошибка.");
        }
    };

    if ($('.vault-files').length > 0) {
        var currentContainer = null;
        var containers = {};
        $.post(
            '/vault/fileshandler', 
            {
                action: 'getContainersList'
            }, 
            function (data) {
                if (!data.success) {
                    showError(data);
                    return;
                }
                if (data.result) {                  
                    var str = '', length = data.result.length;
                    for (var i = 0; i < length; ++i) {
                        containers[data.result[i].name] =  data.result[i];
                        str += '<tr><td><a class="container-link" href="#' + 
                            data.result[i].name + '">' + data.result[i].name + 
                            '</a></td></tr>';
                    }
                    $('.containers-list tbody').html(str);
                }
            }, 
            "json"
        );
        $('.containers-list').on('click', 'a.container-link', function () {
            var container = $(this).attr('href').substring(1);
            if (!container) {
                return;
            }
            currentContainer = container;
            $.post(
                '/vault/fileshandler',
                {
                    action: 'getContainerItems',
                    container: container
                },
                function (data) {
                    if (!data.success) {
                        showError(data);
                        return;
                    }
                    if (data.result) {
                        var str = '', length = data.result.length;
                        for (var i = 0; i < length; ++i) {
                            var c = data.result[i];
                            str += '<tr><td><a href="' + 
                                containers[container]['cdn_uri'] + '/' + 
                                c.name + '" target="_blank">' + c.name + 
                                '</a></td><td>' + c['bytes'] +
                                '</td><td>' + c['content_type'] +
                                '</td><td>' + c['last_modified'] +
                                '</td></tr>';
                        }
                        $('.files-list tbody').html(str);
                        $('.files-panel').show();
                        $('#file-container').val(currentContainer);
                    }
                },
                "json"
            );
            return false;
        });

        $('#file-upload').change(function () {
            if (this.files.length) {
                $('#file-name').val(this.files[0].name);
            }
        });
        $('#file-button').click(function () {
            $('#file-upload').click();
            return false;
        });
    }
});
