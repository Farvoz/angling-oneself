# Angling Oneself

## Команды разработки
- `npm start` — elm-live с горячей перезагрузкой и отладкой

## Production сборка
- `elm make src/Main.elm --output=public/elm.js`

## Деплой
- Пуш в `master` запускает GitHub Actions (`.github/workflows/deploy.yml`)
- URL: https://farvoz.github.io/angling-oneself/

## Архитектура
- Точка входа: `src/Main.elm` → `Browser.element`
- Модули: Main, Model, View, ViewGameElements, ViewHud, Styles, ThemeTokens
- Билд в `public/elm.js` (gitignored)
- Тесты не настроены (elm.json: test-dependencies пуст)