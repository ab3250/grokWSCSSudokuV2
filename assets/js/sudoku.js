// { 
//     "cells": [
//         {
//         "num" : 0,
//         "possibles" : [1,2,3,4,5,6,7,8,9]
//     },
//     {
//         "num" : 0,
//         "possibles" : [1,2,3,4,5,6,7,8,9]
//     } 
//  ]   
// }

let ws
let allCells
// let innercell = `<table id="gridi">
// <tr>
// <td id="cell-i0">1</td>
// <td id="cell-i1">2</td>
// <td id="cell-i2">3</td>
// </tr>
// <tr>
// <td id="cell-i3">4</td>
// <td id="cell-i4">5</td>
// <td id="cell-i5">6</td>
// </tr>
// <tr>
// <td id="cell-i6">7</td>
// <td id="cell-i7">8</td>
// <td id="cell-i8">9</td>
// </tr>
// </table>`
let createWs = function (){
  ws = new WebSocket("ws://localhost:8081")
  console.log("initialized websocket")
  //while (ws.readyState !== 1);
  ws.onopen = function(evt) {
    console.log(evt.type)
    console.log("connected");
  }
  ws.onclose = function() {
    console.log("closed websocket")
    ws.send("close")
    ws.close()
  }
  ws.onmessage = function(evt) {  
    messageHandler(evt)
  }
}

function messageHandler(evt){
//console.log(evt).toString() //data: '[{"type":"grid"},{"num":"5300700006001950000980000…3400803001700020006060000280000419005000080079"}]'
// const d = JSON.parse(evt.data) 
//   switch (d[0].type){
//     case 'grid': 
//       const gridNumbers =  d[1].num.toString() 
//       const possArray  =  d[3].num 
//      //const n = d[1].num.toString()     
//       for (var i = 0; i < gridNumbers.length; i++) {
//         const e = document.getElementById("cell-" + i.toString())
//         if(gridNumbers[i] === '0'){
//           e.innerHTML = getinner(possArray[i], i)          
//           e.disabled = false
//         }
//         else{
//         	e.innerHTML=gridNumbers[i]
//         	e.disabled = true
//         } 
//       }
//       break    
//     case 'lock': 
//       Array.from(document.getElementsByTagName('button')).forEach(function (value, i, col) {
//         value.disabled = true })
//       break
//     case 'unlock':
//       Array.from(document.getElementsByTagName('button')).forEach(function (value, i, col) {
//         value.disabled = false })
//       break
// 	 case 'close':
//   //   allCells = d[1].num 
//          ws.send("close") 

// 	break
//   default: 
// 	break
//   }
//ws.send("hello")
}

function sleep(milliseconds) {
    var start = new Date().getTime();
    for (var i = 0; i < 1e7; i++) {
      if ((new Date().getTime() - start) > milliseconds){
        break;
      }
    }
  }

function mode (btnID) {
   ws.send(btnID) 
}


function loadWindow (){
  Array.from(document.getElementsByTagName('button')).forEach(function (value, i, col) {
    col[i].onclick = function (e) { mode(e.target.id) }
  })
  createWs()
}
document.addEventListener('DOMContentLoaded', loadWindow, false)


function getinner (possArray,index){       
  let temp = []
  for (var  j= 1; j < 10; j++) {
    let double=(j===3 || j===6 )
    temp.push((((j===1)?("<table id=\"cellinner-" + index + "\"><tbody><tr>") :"") + 
                         "<td id=\"cellinnertd-" + (j).toString() + "\">" + (possArray.includes(j) ? j.toString() : "") + "</td>" + 
                         ((j===3 || j===6)?"</tr><tr>":"") +
                         ((j===9)?"</tr></tbody></table>" :"")))        
        }      
       return(temp.join(""))
}

