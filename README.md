# Angling Oneself

Пошаговая браузерная игра на [Elm](https://elm-lang.org/), хостится на GitHub Pages.

## Требования

- [Elm](https://guide.elm-lang.org/install/elm.html) 0.19 (инсталлятор или через npm: `npm install -g elm`).

## Локальный запуск
   ```bash
   elm reactor
   ```

   Затем откройте указанный в консоли адрес (обычно http://localhost:3000).

## Деплой на GitHub Pages

1. Убедитесь, что репозиторий на GitHub и в нём есть ветка `main`.
2. В репозитории: **Settings → Pages → Build and deployment → Source** выберите **GitHub Actions**.
3. При каждом пуше в `main` workflow соберёт Elm-приложение и задеплоит его на Pages.

Сайт будет доступен по адресу: `https://<username>.github.io/angling-oneself/`.
