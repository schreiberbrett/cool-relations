# Graph Editor

```html
<script src="graph-editor.js"></script>
```

First of all, we need an area that, when clicked, renders a node of the graph.

```html
<svg height="300" width="300" onClick="addVertex(event)">

</svg>
```

```js
// @ts-check
```


```js
/**
 * @param {PointerEvent} event
 */
function addVertex(event) {
    const svgElement = event.target;
    
    const clickX = event.clientX - svgElement.getBoundingClientRect().left;
    const clickY = event.clientY - svgElement.getBoundingClientRect().top;

    
    const svgNS = 'http://www.w3.org/2000/svg';
    const circle = document.createElementNS(svgNS, 'circle');

    circle.setAttribute('cx', clickX);
    circle.setAttribute('cy', clickY);
    circle.setAttribute('r', 15);
    circle.setAttribute('stroke', 'black');
    circle.setAttribute('fill', 'white');
    
    console.log(circle);
    console.log(event.target);
    svgElement.appendChild(circle);
}
```

