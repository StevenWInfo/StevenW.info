//Intended to fix sizing issue with scrollbar. Don't know enough yet to make it work. Maybe later.
var checkScrollBars = function(){
    var b = $('body');
    var normalw = 0;
    var scrollw = 0;
    if(b.prop('scrollHeight')>b.height()){
        normalw = window.innerWidth;
        scrollw = normalw - b.width();
        $('#container').css({marginRight:'-'+scrollw+'px'});
    }
}
