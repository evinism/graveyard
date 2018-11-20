let a = Array(100000).fill(1).map(() => [])

let i = 0;

function addOne(){
  i++;
}

Array.prototype.myForEach = function(cb){
  for(let j = 0; j < this.length; j++) {
    cb(this[j]);
  }
}

MyForEach = () => {
  a.myForEach(addOne)
}

ForEach = () => {
  a.forEach(addOne)
}

ForLoop = () => {
  for(let j = 0; j < a.length; j++) {
    addOne();
  }
}

(function TimeIt(){
  for(let i = 0; i < 100; i++){
    MyForEach();
    ForLoop();
    ForEach();
  }
})();
