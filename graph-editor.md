# Graph Editor

```html
<script src="graph-editor.js" />
```

First of all, we need an area that, when clicked, renders a node of the graph.

```html
<svg onclick="addVertex">

</svg>
```

```js
// @ts-check

function addVertex(event) {
    console.log('Clicked!')
}
```
