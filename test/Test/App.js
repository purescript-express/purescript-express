// module Test.App

exports.createMockApp = function() {

    var properties = {
        stringProperty: "string",
        intProperty: 42,
        floatProperty: 100.1,
        booleanProperty: true,
        booleanFalseProperty: false,
        arrayProperty: ["a", "b", "c"],
        emptyArrayProperty: []
    };

    return {
        get: function(propertyName) {
            return properties[propertyName];
        },
        set: function(propertyName, value) {
            properties[propertyName] = value;
        },
    }
}
