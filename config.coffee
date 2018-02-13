exports.salt = "Dev salt"

exports.nrdb_auth_url = "https://netrunnerdb.com/oauth/v2/auth"
exports.nrdb_token_url = "https://netrunnerdb.com/oauth/v2/token"
exports.nrdb_decks_url = "https://netrunnerdb.com/api/2.0/private/decks"
exports.nrdb_deck_url = "https://netrunnerdb.com/api/2.0/private/deck"
exports.nrdb_deck_save_url = "https://netrunnerdb.com/api/2.0/private/deck/save"
exports.nrdb_client_id = process.env.NRDB_CLIENT_ID
exports.nrdb_secret = process.env.NRDB_SECRET
exports.nrdb_callback_url = process.env.NRDB_CALLBACK_URL
exports.nrdb_encryption_key = process.env.NRDB_ENCRYPTION_KEY
