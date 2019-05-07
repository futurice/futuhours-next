#!/bin/sh

docker build -t futurice/futuhours-next:$(git log --pretty=format:'%h' -n 1) .
appswarm image:push -i futurice/futuhours-next -t $(git log --pretty=format:'%h' -n 1)
appswarm app:deploy -i futurice/futuhours-next -t $(git log --pretty=format:'%h' -n 1) -n hours-ui