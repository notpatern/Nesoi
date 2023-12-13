using UnityEngine;

public class EnemyProjectile : MonoBehaviour
{
    [SerializeField] GameObject projectile;
    [SerializeField] private float projectileForce;
    private GameObject bullet;
    private Rigidbody rb;

    public void ThrowProjectile(Transform aimingDevice)
    {
        bullet = Instantiate(projectile, aimingDevice.position, Quaternion.identity);
        Invoke(nameof(DestroyBullet), 10f);
        bullet.GetComponent<DealDamage>().parent = transform;
        rb = bullet.GetComponent<Rigidbody>();
        rb.AddForce(aimingDevice.forward * projectileForce, ForceMode.Impulse);
    }

    private void DestroyBullet()
    {
        Destroy(bullet);
    }
}
