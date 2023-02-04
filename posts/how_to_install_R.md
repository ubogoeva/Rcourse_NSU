---
title: "Инструкция по установке R и RStudio"
# author: "Elena U"
date: '19 Oct 2022'
output:
  html_document:
    toc: true
---

## На Windows 10, 11

**Шаг 1:** Скачать R с [официального сайта](https://cran.r-project.org/bin/windows/base/).\
Запустить установщик.

**Шаг 2:** Скачать [RStudio](https://www.rstudio.com/products/rstudio/download/) (бесплатную версию) \
Запустить установщик с дефолтными параметрами.

Если вдруг у вас на компьютере юзернейм написан кириллицей, то переходите к разделу [часто встречающихся проблем](#часто-встречающиеся-проблемы). Если же нет, поздравляю с подключением!

## На MacOS

Сразу скажу, что в отличие от Windows и Ubuntu на MacOS не имею опыта установки R. В целом, установка не сильно отличается от Windows.

**Шаг 1:** Скачать R с [официального сайта](https://cran.r-project.org/bin/macosx/).\
Запустить установщик.

**Шаг 2:** Скачать [RStudio](https://www.rstudio.com/products/rstudio/download/) (бесплатную версию)\
Запустить установщик с дефолтными параметрами

Однако, я заметила, что пользователи с процессором M1 сталкиваются с определенными проблемами при установке пакетов. Например, при установке пакета `psych` нужен `gfortran` на устройстве. Можно поискать решения, погуглив, например, 'install gfortran mac M1'. Одно из решений приведено по [ссылке](https://stackoverflow.com/questions/71088796/how-to-install-gfortran-for-macos-monterey-version-12-2-1-m1). Кроме того, по этой [ссылке](https://investcookies.ru/post/datatable_m1/data_table_arm/) рекомендации как компилировать пакет `data.table` для M1 процессора.

## На Ubuntu 20.04, 22.04

**Шаг 1:** Установить r-base:

Для Ubuntu 22.04: https://www.how2shout.com/linux/how-to-install-r-base-ubuntu-22-04-lts-jammy/

Для Ubuntu 20.04: https://linuxize.com/post/how-to-install-r-on-ubuntu-20-04/

**Шаг 2:** Установить зависимости RStudio

Набрать в терминале (вызвать можно с помощью сочетания клавиш Ctrl+Alt+T)

    sudo apt update
    sudo apt install libssl-dev libclang-dev libpq5

**Шаг 3:** Установить RStudio

Скачать [отсюда](https://www.rstudio.com/products/rstudio/download/#download). Выбрать бесплатную версию, соответствующую вашей операционной системе. Запустить терминал, перейти в папку загрузок и запустить: `sudo dpkg -i <DEB_FILE_NAME>`, где `DEB_FILE_NAME` название скачанного .deb файла

**Шаг 4:** Установить необходимые для многих R пакетов зависимости

    sudo apt update
    sudo apt -y install libpng-dev libxml2-dev libxml2 libxrender1 libxtst6 libxi6 libfontconfig1-dev libmagickwand-dev libmagick++-dev
    sudo apt -y install libudunits2-dev libgeos-dev libfontconfig1-dev libharfbuzz-dev libfribidi-dev libfreetype6-dev libtiff5-dev libjpeg-dev libpq-dev cargo libcurl4-openssl-dev

Благодаря этой инструкции мне удалось успешно установить `tidyverse` со всеми его зависимостями на свежую Ubuntu 22.04, так что надеюсь, что у вас тоже все получится!

## Рекомендации по настройке RStudio перед началом работы

Первое, что советую сделать: настроить удаление RData при закрытии R и не восстанавливать при перезапуске.

![Tools -\> Global Options - General -\> Workspace](https://r4ds.hadley.nz/diagrams/rstudio/clean-slate.png){width="559"}

Рекомендую почитать материал по эффективной организации работы в R по [ссылке](https://telegra.ph/R-how-to-organize-work-08-08), разобрано более подробно и с большим количеством ссылок на дополнительные материалы

## Часто встречающиеся проблемы

Одна из распространенных проблем у пользователя R на Windows - это написанное кириллицей имя пользователя.

-   Надежнее всего попробовать переименовать юзернейм например по этой [инструкции](https://altarena.ru/kak-pomenyat-imya-polzovatelya-v-vindovs-10-s-kirillitsy-na-latinitsu/). Можно погуглить еще, как переименовать пользователя в вашей версии Windows, вот еще одна [ссылка](https://www.wintips.org/how-to-rename-user-profile-folder-in-windows-10-8-7/).
-   Также можно переустановить систему с созданием юзернейма на латинице.

Но если эти подходы кажутся сложными и долгими, можно изменить пути для записи устанавливаемых библиотек, чтобы они не содержали кириллицу. Однако, я не гарантирую, что это не повлечет проблем в дальнейшем, но этот путь кажется проще.

Также способ замены дефолтного пути может помочь, если по умолчанию R пытается поставить пакеты в папку OneDrive.

**Шаг 1:** Создайте папку в корневом диске C:// или D:// для установки пакетов без кириллических символов и пробелов, например `C:/Rlib`.

**Шаг 2:** Создайте папку для временных файлов без кириллических символов и пробелов, например `C:/Temp`

**Шаг 3:** Выполните в консоли Rstudio команду

    system("setx R_LIBS C:/Rlib")
    system("setx TEMP C:/Temp")
    system("setx TMP C:/Temp")

**Шаг 4:** Перезапустите RStudio (можно сделать сочетанием клавиш Ctrl+Shift+F10)

**Шаг 5:** Проверьте, что R знает, куда ему ставить пакеты. Для этого выполните в консоли Rstudio команду `.libPaths()`. Скорее всего в выдаче будет две директории, исходная `C:/Users/Юзер/AppData/...`, и вновь созданная, то есть `C:/Rlib`. Чтобы переназначить новую директорию как дефолтную для установки пакетов запустите следующий код:

    myPaths <- .libPaths()   # get the paths
    myPaths <- c(myPaths[2], myPaths[1])  # switch them
    .libPaths(myPaths)  # reassign them

**Шаг 6:** Однако это понадобится делать каждый раз при перезапуске сессии RStudio. Чтобы сделать эту настройку постоянной, можно прописать этот код в файле Rprofile. Найти этот файл можно следующим образом: Tools -\> Global Options -\> General

![здесь содержится путь к актуальной версии R](https://cdn.accelebrate.com/images/library/how-to-articles/r-rstudio-library/r-version.png){width="530"}

Перейти в эту директорию и дальше в `./library/base/R`, найти файл `Rprofile`

![](https://cdn.accelebrate.com/images/library/how-to-articles/r-rstudio-library/rprofile.png){width="459"}

![открыть файл Rprofile текстовым редактором, например Notepad++](https://cdn.accelebrate.com/images/library/how-to-articles/r-rstudio-library/r-code.png){width="554"}

В конце файла вставить строки, переназначающие нашу дефолтную библиотеку:

    myPaths <- .libPaths()
    myPaths <- c(myPaths[2], myPaths[1])
    .libPaths(myPaths)

Перезапустите RStudio. Надеюсь, это поможет успешно устанавливать пакеты и осваивать R!

Небольшое напоминание - не рекомендуется использовать кириллицу и пробелы в названиях столбцов и файлов, потому что могут быть ошибки с кодировкой при пересылании файла, в результате чего кириллица превратится в `???`. Исправить это можно с помощью File - Reopen with encoding, выбрать UTF-8 в качестве кодировки. Но в целом рекомендую даже комментарии, не говоря уж об остальном, писать только латиницей, это позволит избежать проблем с кодировкой в будущем.

При написании этого материала использовала следующие источники:

1.  https://bdemeshev.github.io/installation/r/R_installation.html
2.  https://www.accelebrate.com/library/how-to-articles/r-rstudio-library
3.  Материалы со школы [NGSchool](https://ngschool.eu/ngschool2022/)

Подписывайтесь на мой [телеграм-канал](https://t.me/stats_for_science), следите за новостями, обещаю интересные материалы по статистике и R!
