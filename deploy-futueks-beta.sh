# !/bin/bash
# Script for updating a deployment of an existing service
# - a new service requires 1) YAML pushed first 2) ingress configured
# COPY AND MODIFY TO SUIT YOUR NEEDS
set -eou pipefail

NAME=betahours # this is your service name :)
FUTUEKS_NAMESPACE=app # modify this to match target namespace (app, play, exp, ...)
AWS_PROFILE=$FUTUEKS_NAMESPACE
export AWS_PROFILE=$FUTUEKS_NAMESPACE
KUBECONFIG=/tmp/kubeconfig
AWS_REGION=eu-central-1

echo ""
echo "Before proceeding:"
echo "1. Setup access configuration, instructions at https://welcome.$FUTUEKS_NAMESPACE.futurice.com; substitute PROFILE-NAME with $AWS_PROFILE"
echo "2. aws sso login --profile $AWS_PROFILE"
echo ""

bye() {
    echo "Bye..."
    [[ "$0" = "$BASH_SOURCE" ]] && exit 1 || return 1 # handle exits from shell or function but don't exit interactive shell
}

replaceinfile() {
    find $1 -type f -exec sed -i.bak "s~$2~$3~g" {} \;
    rm -f $1.bak
}

read -p "Do you want to deploy (y/n)? " -n 1 -r
echo ""
if [[ ! $REPLY =~ ^[Yy]$ ]]; then
    bye
fi

echo "* running fix-eslint"
npm run fix-eslint

echo "* running fix-prettier"
npm run fix-prettier

echo "* running eslint"
npm run lint

echo "* fetching kubeconfig"
aws s3 cp s3://futueks.futueks.futurice.com/$FUTUEKS_NAMESPACE/kubeconfig $KUBECONFIG --profile $AWS_PROFILE
replaceinfile $KUBECONFIG PROFILE-NAME $FUTUEKS_NAMESPACE

TAG=$(git rev-parse --short HEAD)

echo "* building Docker image $NAME:$TAG"
docker build -t $NAME:$TAG .

echo "* pushing image to private registry"
REPO_NAME="$FUTUEKS_NAMESPACE/$NAME"
aws ecr create-repository --repository-name $REPO_NAME --region $AWS_REGION||true
REPO=$(aws ecr describe-repositories --repository-names $REPO_NAME --region=$AWS_REGION|jq -r '.repositories[].repositoryUri')
docker tag $NAME:$TAG $REPO:$TAG
aws ecr get-login-password --region $AWS_REGION|docker login --username AWS --password-stdin $REPO
docker push $REPO:$TAG
echo "* image: $REPO:$TAG"

echo "* updating deployment image"
kubectl set image deployments/$NAME -n $FUTUEKS_NAMESPACE $NAME=$REPO:$TAG --kubeconfig $KUBECONFIG

echo "* kubectl get pods -n $NAME -n $FUTUEKS_NAMESPACE --kubeconfig $KUBECONFIG"
kubectl get pods -n $NAME -n $FUTUEKS_NAMESPACE --kubeconfig $KUBECONFIG|grep $NAME
