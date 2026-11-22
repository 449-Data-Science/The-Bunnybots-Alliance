function resizeFrame() {

    var frame = window.frameElement;

    if (!frame) {
        console.log("No iframe detected");
        return;
    }

    var activeTab = document.querySelector('.tab-pane.active');

    var navbar = document.querySelector('.navbar');
    var navHeight = navbar ? navbar.offsetHeight : 0;

    var contentHeight;

    if (activeTab) {
        contentHeight = activeTab.getBoundingClientRect().height;

        // Include overflowing content
        contentHeight = Math.max(
            contentHeight,
            activeTab.scrollHeight,
            activeTab.offsetHeight
        );
    } else {
        contentHeight = document.documentElement.scrollHeight;
    }

    var height = Math.ceil(contentHeight + navHeight + 30);

    console.log(
        "Resizing iframe:",
        height,
        "px | tab:",
        activeTab
    );

    frame.style.height = height + "px";
    frame.style.minHeight = height + "px";
    frame.style.overflow = "hidden";

    document.documentElement.style.overflow = "hidden";
    document.body.style.overflow = "hidden";
}


/* ---------------------------------------------------------
   INITIAL LOAD
--------------------------------------------------------- */

window.addEventListener("load", function() {
    setTimeout(resizeFrame, 500);
    setTimeout(resizeFrame, 1500);
    setTimeout(resizeFrame, 3000);
});


/* ---------------------------------------------------------
   TAB CHANGES
--------------------------------------------------------- */

document.addEventListener("click", function(e) {

    var tab = e.target.closest(
        'a[data-bs-toggle="tab"], a[data-toggle="tab"]'
    );

    if (tab) {
        setTimeout(resizeFrame, 100);
        setTimeout(resizeFrame, 500);
        setTimeout(resizeFrame, 1500);
    }
});


/* ---------------------------------------------------------
   SHINY OUTPUTS
--------------------------------------------------------- */

$(document).on(
    "shiny:value shiny:outputinvalidated",
    function() {
        setTimeout(resizeFrame, 200);
        setTimeout(resizeFrame, 1000);
    }
);


/* ---------------------------------------------------------
   SHINY IDLE
--------------------------------------------------------- */

$(document).on("shiny:idle", function() {
    setTimeout(resizeFrame, 500);
});


/* ---------------------------------------------------------
   TABLE SEARCH BOXES
   Each .table-search div names its table in data-target.
--------------------------------------------------------- */

$(document).on("input", ".table-search input", function() {

    var tableId = $(this).closest(".table-search").data("target");
    var value = this.value;

    var $table = $("#" + tableId).find("table.dataTable").filter(function() {
        return $.fn.dataTable.isDataTable(this);
    });

    if ($table.length) {
        $table.DataTable().search(value).draw();
        // Fewer or more rows changes the page height, so resize the iframe
        setTimeout(resizeFrame, 100);
        setTimeout(resizeFrame, 500);
    }
});


/* ---------------------------------------------------------
   MATCH CLICK-THROUGH
   Clicking the Match cell of a results table asks the server
   to open that match on the Matches tab. The label is the cell
   text: a number for qualifications, the match name for playoffs.
--------------------------------------------------------- */

$(document).on("click", "table.match-table td.match-cell", function() {
    var label = $(this).text().trim();
    if (!label || $(this).closest("tr").hasClass("pending")) { return; }
    Shiny.setInputValue("goto_match", { label: label, nonce: Date.now() }, { priority: "event" });
});

/* updateNavbarPage() switches tabs without a click, so resize here too */
$(document).on("shown.bs.tab", function() {
    setTimeout(resizeFrame, 100);
    setTimeout(resizeFrame, 600);
});


/* ---------------------------------------------------------
   TEAM CLICK-THROUGH
   Any element with class .team-cell (table cell or lineup chip) holds a team number.
   Clicking it opens that team on the Teams tab.
--------------------------------------------------------- */

$(document).on("click", ".team-cell", function() {
    var team = $(this).text().trim();
    if (!/^[0-9]+$/.test(team)) { return; }   // empty cells show a dash
    Shiny.setInputValue("goto_team", { team: team, nonce: Date.now() }, { priority: "event" });
});

/* ---------------------------------------------------------
   BANNER CLICK
   Clicking the title banner ("The Bunnybots Alliance: ...") goes back to the
   Results tab, like the logo link on most sites.
--------------------------------------------------------- */

$(document).on("click", ".brand-full", function(e) {
    e.preventDefault();   // the brand is a "#" link; don't jump the page
    $('.navbar-nav a[data-value="Results"]').first().trigger("click");
    window.scrollTo({ top: 0 });
});