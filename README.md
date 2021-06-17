FutuHours-next
=========

Application for updating and editing employee hours, all new and re-written in Elm and elm-ui.

Currently in Beta and deployed at [betahours](https://betahours.app.futurice.com) (internal link only).

**Requirements** 

- npm
- [create-elm-app](https://github.com/halfzebra/create-elm-app)
  - version 4.5.0 is confirmed to work
- Docker (for deploying)
- [Futuswarm](https://github.com/futurice/futuswarm) (for deploying)
  - Futurice employees only use [Appswarm](https://futuswarm-mainpage.app.futurice.com/)

You will also need a running backend API server on the same host at port 8803, for which you will need to set up [haskell-mega-repo](https://github.com/futurice/haskell-mega-repo). The backend server code is in [hours-api](https://github.com/futurice/haskell-mega-repo/tree/master/hours-api). 

For development, it is sufficient to get as far as being able to run `cabal new-run hours-mock-server`. If the port the backend server is running on is not 8803, you can change the proxy setting to point to the correct address in `elmapp.config.js`.

**Development**

To start the development environment, do:

```
$ elm-app start
```


**Git Workflow**

The repo contains two main branches: `master`, which is the current production version which should be deployed to `hours`, and `develop` which is the current development version and may be deployed to `betahours`. 

Feature branches, fixes, and PRs should be made against `develop`, and releases are handled as a PR from `develop` to `master`, with a corresponding deployment to `hours`.

**Deployment**

To deploy, use the following commands, where `<server-id>` is replaced with either `betahours` for the beta server, or `hours` for production:

``` 
$ docker build -t futurice/futuhours-next:$(git log --pretty=format:'%h' -n 1) .
$ appswarm image:push -i futurice/futuhours-next -t $(git log --pretty=format:'%h' -n 1)
$ appswarm app:deploy -i futurice/futuhours-next -t $(git log --pretty=format:'%h' -n 1) -n <server-id>
```

Alternately, simple bash scripts are provided to save you typing:

```
./deploy-beta.sh
./deploy-prod.sh
```

**Environment variables**

There are two environment variables needed to run this in a container.

| ENV variable | Example value | Description                               |
|--------------|---------------|-------------------------------------------|
| API_HOST     | http://hours-api.app   | The API url |
| ELM_APP_FUTUCORTEX_IFRAME_URL         | https://futucortex.play.futurice.com/embed-futurice-proposals              | The URL of Futucortex to render in the iframe|

**License**

Copyright 2019 by Futurice Oy

[Apache License 2.0](LICENSE)
