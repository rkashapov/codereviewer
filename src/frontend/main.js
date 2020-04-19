import { Elm } from './Main.elm';

document.addEventListener('DOMContentLoaded', () => {
    Elm.Main.init({
        node: document.getElementById('app'),
        flags: location.origin + '/gql',
    });
});