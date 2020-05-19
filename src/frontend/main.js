import { Elm } from './Main.elm';

document.addEventListener('DOMContentLoaded', () => {
    Elm.Main.init({
        node: document.getElementById('app'),
        flags: {
            apiUrl: location.origin + '/gql',
            width: window.innerWidth,
            height:window.innerHeight,
        },
    });
});