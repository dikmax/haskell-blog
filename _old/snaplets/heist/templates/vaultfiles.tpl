<!DOCTYPE html>
<html lang="ru">
<apply template="vaulthead"/>
<body>
<apply template="vaulttopnav"/>
<div class="container vault-files">
    <a class="btn" href="/vault">Список записей</a>
    <div class="row">
        <div class="span3">
            <table class="containers-list table table-stripped table-hover">
                <thead>
                    <tr><th>Контейнеры</th></tr>
                </thead>
                <tbody></tbody>
            </table>
        </div>
        <div class="span9 files-panel" style="display: none;">
            <form class="well form-inline upload-form" action="/vault/fileupload" enctype="multipart/form-data" method="post">
                <strong>Загрузить: </strong>
                <input type="hidden" name="container" id="file-container" />
                <input type="file" name="file" style="position: absolute; top: -100px; left: -100px;" id="file-upload" />
                <button class="btn" type="button" id="file-button">Выбрать</button>
                <input type="text" name="name" id="file-name" placeholder="Под именем" />
                <button class="btn" id="file-submit">Отправить</button>
            </form>
            <table class="files-list table table-stripped table-hover">
                <thead>
                    <tr>
                        <th>Имя</th>
                        <th>Размер</th>
                        <th>Тип</th>
                        <th>Дата</th>
                    </tr>
                </thead>
                <tbody></tbody>
            </table>
        </div>
    </div>
</div>
<apply template="vaultfoot"/>
</body>
</html>