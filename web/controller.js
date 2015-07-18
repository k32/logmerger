var logViewerApp = angular.module('logViewerApp', ['ui.bootstrap', 
                                                   'infinite-scroll']);

logViewerApp.controller('EntriesCtrl', function ($scope, $modal, $http) {
    $scope.openDump = function() {
        console.log("Opening a dump...");
        
        var openDumpDialogInstance = $modal.open({
            animation: $scope.animationsEnabled,
            templateUrl: 'OpenDumpDialog.html',
            controller: 'OpenDumpCtrl',
            resolve: {
                items: function () {
                    return $scope.items;
                }
            }
        });
   
        openDumpDialogInstance.result.then(
            function (dumpReq) {
                var data;
                var type;
                if(dumpReq.type == 'url') {
                    data=dumpReq.url;
                }
                $http.post("/api/dumps/", data).success(function(response) {
                    console.log("Ok, got initial data");
                    $scope.logEntries = response.entries;
                });
                
            },
            function () {}
        );
    };

    $scope.logEntries = [];
    
    $scope.isCollapsed = false;
});

logViewerApp.controller('OpenDumpCtrl', function ($scope, $modalInstance) {
    $scope.openUrl = function () {
        console.log("Opening url: ");
        console.log($scope.url);
        $modalInstance.close({ type: 'url', url: $scope.url });
    };

    $scope.cancel = function () {
        $modalInstance.dismiss('cancel');
    };
});
