function toggleDescription(projectId) {
    var description = document.getElementById(projectId);
    var btn = document.getElementById(projectId + "_btn");
    
    if (description.style.display === "none" || description.style.display === "") {
        description.style.display = "block";
        btn.innerHTML = "Lire moins";
    } else {
        description.style.display = "none";
        btn.innerHTML = "Lire plus";
    }
}