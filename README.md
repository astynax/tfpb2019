### TFPB'19.

Материалы по оффлайн-курсу по "Typed FB Basics".

![logo](./assets/tfpb.png)


#### Что лежит в папках

Каждая лекция находится в своей папке. Папки пронумерованы. Внутри каждой папки есть файлики:
- `*.pdf` - пдфка с лекцией
- `*.tex` - теховский файлик, из которого собралась пдфка
- `code.hs` - код с лекции

Текст лекции (кроме первой) записан в формате <img src="https://latex.codecogs.com/gif.latex?\LaTeX" /> [ссылка](https://ru.wikipedia.org/wiki/TeX). Для его превращения к ПДФ необходимо выполнить 2 действия:
- Скачать пакет для работы с `tex` для своей операционной сисемы. (оно гуглится довольно быстро. Для ubuntu - это один из пакетов `texlive`. Для windows - MiKTeX и т.д.).
- Собрать `PDF` при помощи команды `pdflatex %FILE_NAME%`. Если не собирается - скорее всего нет необходимого языкового пакета. Нужный можно найти по тексту ошибки.

Рядом с текстом лекций лежит файл `code.hs`. Это примеры кода, записанные на лекции. В сущности этот файл не несет особенно полезной нагрузки, так как те же примеры кода есть и в самоу лекции.

Каждая папка может содержать дополнительные листинги с кодом. Их описание приведено ниже в разделе "Прошедшие занятия".


#### Прошедшие занятия.

1. Алгебраические типы данных
    - Синтаксис языка Haskell: [LHS](01_algebraic_data_types/syntax.lhs)+[PDF](01_algebraic_data_types/syntax.pdf)
    - Алгебраические типы данных [adt.hs](01_algebraic_data_types/adt.hs)
    - [Задание про фишки #1](https://gist.github.com/astynax/1eb88e195c4bab2b8d31d04921b18dd0). Пример реализации [columns.hs](01_algebraic_data_types/columns.hs)
2. Функции в языке `haskell`
    - Задание про фишки #2 (https://gist.github.com/astynax/1eb88e195c4bab2b8d31d04921b18dd0). Пример реализации [move4x4.hs](02_functions_in_haskell/move4x4.hs)
3. Pattern matching
    - Примеры паттерн матчинга на листах [list_stuff.hs](03_pattern_matching/list_stuff.hs)
    - Задания:
        - Реализовать функции `map`, `filter`, `zip`, `zipWith` используя `foldl`
        - Реализовать функцию, которая по набору "высок точек", выведет на экран рельеф, заполненный водой. Более подробна задача описана в .tex файле в самом конце. Пример реализации: [terrain_after_rain.hs](03_pattern_matching/terrain_after_rain.hs)
4. Классы типов, Моноид
    - Классы типов: [LHS](04_typeclasses/typeclasses.lhs)+[PDF](04_typeclasses/typeclasses.pdf)
    - Полугруппа и моноид: [LHS](04_typeclasses/monoid.lhs)+[PDF](04_typeclasses/monoid.pdf)
5. Records, Functor
    - Records: [LHS](05_records_and_hkt/records.lhs)+[PDF](05_records_and_hkt/records.pdf)
    - HKT, Functor, Bifunctor, Foldable: [LHS](05_records_and_hkt/functor.lhs)+[PDF](05_records_and_hkt/functor.pdf)
