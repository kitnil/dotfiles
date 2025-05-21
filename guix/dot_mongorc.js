search = function(id) {
    return db.getMongo().getDBNames().map((database) => {
        let sibling = db.getSiblingDB(database);
        return sibling.getCollectionNames().map((collection) => sibling.getCollection(collection).findOne({"_id" : ObjectId(id)})).filter((notNull) => notNull);
    }).filter(String)
}

variety = function(id) {
    return db.getMongo().getDBNames().map((database) => {
        let sibling = db.getSiblingDB(database);
        sibling.getCollectionNames().map((collection) => {
            return (typeof(sibling.getCollection(collection)))
        })
    }).filter(String)
}
