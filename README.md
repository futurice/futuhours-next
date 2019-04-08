FutuHours-next
=========

Application for updating and editing employee hours, all new and re-written in Elm and elm-ui.

**Requirements** 

- npm
- [create-elm-app](https://github.com/halfzebra/create-elm-app)
- Docker (for deploying)
- [Appswarm](https://futuswarm-mainpage.app.futurice.com/) (for deploying)

**Development**

To start the development environment, do:

```
$ elm-app start
```

**Deployment**

To deploy:

``` 
$ docker build -t futurice/hours-ui:$(git log --pretty=format:'%h' -n 1) .
$ appswarm image:push -i futurice/hours-ui -t $(git log --pretty=format:'%h' -n 1)
$ appswarm app:deploy -i futurice/hours-ui -t $(git log --pretty=format:'%h' -n 1) -n http-hello
```

**License**

[Apache License 2.0](LICENSE)