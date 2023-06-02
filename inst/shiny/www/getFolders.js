window.pressed = function(){
        var a = document.getElementById('fileIn');
        if(a.value === "")
        {
            noFile.innerHTML = "No folder choosen";
        }
        else
        {
            noFile.innerHTML = "";
        }
    };

document.getElementById("fileIn").addEventListener("change", function(e) {

            //console.log(e.webkitRelativePath);
            //let files = e.target.files;
            //var arr = new Array(files.length*2);
            //for (let i=0; i<files.length; i++) {

            //console.log(files[0].webkitRelativePath);
            //console.log(files[0].name);
            //arr[i] = files[i].webkitRelativePath;
            //arr[i+files.length] = files[i].name;


            //}

            //Shiny.onInputChange("mydata", arr);

             let output = document.getElementById("listing");
  for (const file of e.target.files) {
    let item = document.createElement("li");
    item.textContent = file.webkitRelativePath;
    console.log(item);
  };

    });
