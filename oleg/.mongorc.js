search = function(id) {
    return db.getMongo().getDBNames().map((database) => {
        let sibling = db.getSiblingDB(database);
        return [sibling, sibling.getCollectionNames().map((collection) => sibling.getCollection(collection).findOne({"_id" : ObjectId(id)})).filter((notNull) => notNull)];
    })
}
